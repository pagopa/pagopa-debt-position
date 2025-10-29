package it.gov.pagopa.debtposition.scheduler;

import static it.gov.pagopa.debtposition.util.SchedulerUtils.*;

import com.azure.data.tables.TableClient;
import com.azure.data.tables.TableClientBuilder;
import com.azure.data.tables.models.TableEntity;
import com.azure.data.tables.models.TableTransactionAction;
import com.azure.data.tables.models.TableTransactionActionType;
import com.azure.data.tables.models.TableTransactionFailedException;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import it.gov.pagopa.debtposition.entity.PaymentOption;
import it.gov.pagopa.debtposition.entity.PaymentPosition;
import it.gov.pagopa.debtposition.repository.PaymentPositionRepository;
import java.net.URISyntaxException;
import java.security.InvalidKeyException;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.stream.Collectors;
import jakarta.persistence.EntityManager;
import jakarta.persistence.EntityManagerFactory;
import jakarta.persistence.PersistenceUnit;
import jakarta.transaction.Transactional;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import net.javacrumbs.shedlock.spring.annotation.SchedulerLock;
import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

@Component
@Slf4j
@ConditionalOnProperty(name = "cron.job.schedule.history.enabled", matchIfMissing = true)
@NoArgsConstructor
public class HistoricizationScheduler {

  private static final String LOG_BASE_HEADER_INFO =
      "[OperationType: %s] - [ClassMethod: %s] - [MethodParamsToLog: %s]";
  private static final String CRON_JOB = "CRON JOB";
  private static final String METHOD = "manageDebtPositionsToHistoricize";
  @Getter private Thread threadOfExecution;

  // extraction params
  @Value(
      "${cron.job.schedule.history.query:SELECT pp FROM PaymentPosition pp WHERE pp.status IN"
          + " ('PAID', 'REPORTED', 'INVALID', 'EXPIRED') AND pp.lastUpdatedDate < ?1}")
  private String extractionQuery;

  @Value("${cron.job.schedule.history.query.interval:365}")
  private short extractionInterval;

  // extraction params: pagination mode
  @Value("${cron.job.schedule.history.paginated:true}")
  private boolean paginationMode;

  @Value(
      "${cron.job.schedule.history.query.count:SELECT count(pp.id) FROM PaymentPosition pp WHERE"
          + " pp.status IN ('PAID', 'REPORTED', 'INVALID', 'EXPIRED') AND pp.lastUpdatedDate < ?1}")
  private String countExtractionQuery;

  @Value("${cron.job.schedule.history.query.page.size:10000}")
  private int pageSize;

  // azure storage params
  @Value("${azure.archive.storage.connection}")
  private String archiveStorageConnection;

  @Value("${azure.archive.storage.table.pp:paymentpositiontable}")
  private String archiveStoragePPTable;

  @Value("${azure.archive.storage.batch.operation.size:100}")
  private short maxBatchOperationSize;

  @Autowired private PaymentPositionRepository paymentPositionRepository;
  @PersistenceUnit private EntityManagerFactory emf;

  public HistoricizationScheduler(PaymentPositionRepository paymentPositionRepository) {
    super();
    this.paymentPositionRepository = paymentPositionRepository;
  }

  @Scheduled(cron = "${cron.job.schedule.history.trigger}")
  @SchedulerLock(
      name = "HistoricizationScheduler_manageDebtPositionsToHistoricize",
      lockAtMostFor = "${cron.job.schedule.history.shedlock.lockatmostfor}",
      lockAtLeastFor = "${cron.job.schedule.history.shedlock.lockatleastfor}")
  public void manageDebtPositionsToHistoricize()
      throws JsonProcessingException, InvalidKeyException, URISyntaxException {
    updateMDCForStartExecution("manageDebtPositionsToHistoricize", "");
    try {
      log.debug(
          String.format(
              LOG_BASE_HEADER_INFO,
              CRON_JOB,
              METHOD,
              "Running at "
                  + DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")
                      .format(LocalDateTime.now())));
      EntityManager em = this.getEntityManager();
      LocalDateTime ldt = LocalDateTime.now().minusDays(extractionInterval);
      List<PaymentPosition> ppList;
      if (paginationMode) {
        long countResult =
            em.createQuery(countExtractionQuery, Long.class).setParameter(1, ldt).getSingleResult();
        int numOfPages = (int) Math.ceil((float) countResult / pageSize);
        for (int pageNumber = 0; pageNumber < numOfPages; pageNumber++) {
          ppList =
              em.createQuery(extractionQuery, PaymentPosition.class)
                  .setParameter(1, ldt)
                  .setFirstResult((pageNumber) * pageSize)
                  .setMaxResults(pageSize)
                  .getResultList();
          log.debug(
              String.format(
                  LOG_BASE_HEADER_INFO,
                  CRON_JOB,
                  METHOD,
                  "Paginated historical extraction info: Found n. "
                      + countResult
                      + " debt positions to archive splitted on n. "
                      + numOfPages
                      + " of pages. "
                      + System.lineSeparator()
                      + "Page number "
                      + pageNumber
                      + " has been extracted and contains "
                      + ppList.size()
                      + " occurrences"));
          this.archiveAndDeleteDebtPositions(ppList);
        }
      } else {
        ppList =
            em.createQuery(extractionQuery, PaymentPosition.class)
                .setParameter(1, ldt)
                .getResultList();
        log.debug(
            String.format(
                LOG_BASE_HEADER_INFO,
                CRON_JOB,
                METHOD,
                "Total entries historical extraction info: Number of extracted rows to historicize:"
                    + " "
                    + ppList.size()));
        this.archiveAndDeleteDebtPositions(ppList);
      }
      log.debug(
          String.format(
              LOG_BASE_HEADER_INFO,
              CRON_JOB,
              METHOD,
              "Finished at "
                  + DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")
                      .format(LocalDateTime.now())));
      updateMDCForEndExecution();
    } catch (Exception e) {
      updateMDCError(e, "Historicize Scheduler Error");
      throw e;
    } finally {
      MDC.clear();
    }
  }

  public EntityManager getEntityManager() {
    return emf.createEntityManager();
  }

  public TableClient getTableClient(String connectionString, String tableName) {
    return new TableClientBuilder()
        .connectionString(connectionString)
        .tableName(tableName)
        .buildClient();
  }

  public void upsertPPTable(List<PaymentPosition> organizationPpList, ObjectMapper objectMapper)
      throws JsonProcessingException, InvalidKeyException, URISyntaxException {

    TableClient tc = this.getTableClient(archiveStorageConnection, archiveStoragePPTable);
    var transactionActions = new ArrayList<TableTransactionAction>();

    short numOfBatchOperations = 0;
    try {
      for (int i = 0; i < organizationPpList.size(); i++) {
        PaymentPosition pp = organizationPpList.get(i);
        for (int j = 0; j < pp.getPaymentOption().size(); j++) {
          PaymentOption po = pp.getPaymentOption().get(j);
          // bulk operation to persist the PO debt position info
          if (numOfBatchOperations < maxBatchOperationSize - 1
              && i < organizationPpList.size() - 1) {
            TableEntity tableEntity = new TableEntity(pp.getOrganizationFiscalCode(), po.getIuv());
            Map<String, Object> properties = new HashMap<>();
            properties.put("PaymentDate", po.getPaymentDate());
            properties.put("PaymentPosition", objectMapper.writeValueAsString(pp));
            tableEntity.setProperties(properties);
            transactionActions.add(
                new TableTransactionAction(TableTransactionActionType.UPSERT_MERGE, tableEntity));
            numOfBatchOperations++;
          } else {
            TableEntity tableEntity = new TableEntity(pp.getOrganizationFiscalCode(), po.getIuv());
            Map<String, Object> properties = new HashMap<>();
            properties.put("PaymentDate", po.getPaymentDate());
            properties.put("PaymentPosition", objectMapper.writeValueAsString(pp));
            tableEntity.setProperties(properties);
            transactionActions.add(
                new TableTransactionAction(TableTransactionActionType.UPSERT_MERGE, tableEntity));
            tc.submitTransaction(transactionActions);
            log.debug(
                String.format(
                    LOG_BASE_HEADER_INFO,
                    CRON_JOB,
                    "upsertPPTable",
                    "block of n. "
                        + transactionActions.size()
                        + " items are upserted to a total of "
                        + organizationPpList.size()
                        + " for the organization fiscal code "
                        + pp.getOrganizationFiscalCode()));
            // reset for a new bulk operation
            transactionActions = new ArrayList<TableTransactionAction>();
            numOfBatchOperations = 0;
          }
        }
      }

    } catch (TableTransactionFailedException e) {
      log.error(
          String.format(
              LOG_BASE_HEADER_INFO,
              CRON_JOB,
              "upsertPPTable",
              "error while storing the table information [maxBatchOperationSize="
                  + maxBatchOperationSize
                  + ", executedBlockSize="
                  + transactionActions.size()
                  + ", "
                  + "failedTransactionIndex="
                  + e.getFailedTransactionActionIndex()
                  + ", message="
                  + e.getMessage()
                  + "]"),
          e);
      throw e;
    }
  }

  @Transactional
  private void archiveAndDeleteDebtPositions(List<PaymentPosition> ppList)
      throws JsonProcessingException, InvalidKeyException, URISyntaxException {
    this.archivesDebtPositions(ppList);
    // archived debt positions are removed
    paymentPositionRepository.deleteAll(ppList);
    log.debug(
        String.format(
            LOG_BASE_HEADER_INFO,
            CRON_JOB,
            "archiveAndDeleteDebtPositions",
            "deleted n. " + ppList.size() + " archived debt positions"));
  }

  private void archivesDebtPositions(List<PaymentPosition> ppList)
      throws JsonProcessingException, InvalidKeyException, URISyntaxException {
    Map<String, List<PaymentPosition>> ppListByOrganizationFiscalCode =
        ppList.stream()
            .collect(
                Collectors.groupingBy(
                    p -> p.getOrganizationFiscalCode(),
                    Collectors.mapping((PaymentPosition p) -> p, Collectors.toList())));

    ObjectMapper objectMapper = new ObjectMapper();
    objectMapper.registerModule(new JavaTimeModule());

    for (Entry<String, List<PaymentPosition>> entry : ppListByOrganizationFiscalCode.entrySet()) {
      List<PaymentPosition> organizationPpList = ppListByOrganizationFiscalCode.get(entry.getKey());
      this.upsertPPTable(organizationPpList, objectMapper);
      log.debug(
          String.format(
              LOG_BASE_HEADER_INFO,
              CRON_JOB,
              "archivesDebtPositions",
              "historicized n. "
                  + organizationPpList.size()
                  + " debt positions for the organization fiscal code: "
                  + entry.getKey()));
    }
  }
}
