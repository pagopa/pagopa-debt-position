package it.gov.pagopa.debtposition.scheduler;

import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.stream.Collectors;

import javax.persistence.EntityManager;
import javax.persistence.EntityManagerFactory;
import javax.persistence.PersistenceUnit;
import javax.transaction.Transactional;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.scheduling.annotation.Async;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

import com.azure.data.tables.TableClient;
import com.azure.data.tables.TableClientBuilder;
import com.azure.data.tables.models.TableEntity;
import com.azure.data.tables.models.TableErrorCode;
import com.azure.data.tables.models.TableServiceException;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;

import it.gov.pagopa.debtposition.entity.PaymentOption;
import it.gov.pagopa.debtposition.entity.PaymentPosition;
import it.gov.pagopa.debtposition.repository.PaymentPositionRepository;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import net.javacrumbs.shedlock.spring.annotation.SchedulerLock;

@Component
@Slf4j
@ConditionalOnProperty(name = "cron.job.schedule.history.enabled", matchIfMissing = true)
@NoArgsConstructor
public class HistoricizationScheduler {

	private static final String LOG_BASE_HEADER_INFO = "[OperationType: %s] - [ClassMethod: %s] - [MethodParamsToLog: %s]";
    private static final String CRON_JOB = "CRON JOB";
    private static final String METHOD = "manageDebtPositionsToHistoricize";
    @Getter
    private Thread threadOfExecution;
    
    
    
    // extraction params
    @Value("${cron.job.schedule.history.query:SELECT pp FROM PaymentPosition pp WHERE pp.status IN ('PAID', 'REPORTED', 'INVALID', 'EXPIRED') AND pp.lastUpdatedDate < ?1}")
    private String extractionQuery;
    @Value("${cron.job.schedule.history.query.interval:365}")
    private short extractionInterval;
    // extraction params: pagination mode
    @Value("${cron.job.schedule.history.paginated:true}")
    private boolean paginationMode;
    @Value("${cron.job.schedule.history.query.count:SELECT count(pp.id) FROM PaymentPosition pp WHERE pp.status IN ('PAID', 'REPORTED', 'INVALID', 'EXPIRED') AND pp.lastUpdatedDate < ?1}")
    private String countExtractionQuery;
    @Value("${cron.job.schedule.history.query.page.size:100000}")
    private int pageSize;
    
    // azure storage params
    @Value("${azure.archive.storage.connection}")
    private String archiveStorageConnection;
    @Value("${azure.archive.storage.table.po:paymentoptiontable}")
    private String archiveStoragePOTable;
    @Value("${azure.archive.storage.table.pp:paymentpositiontable}")
    private String archiveStoragePPTable;
    
    @Autowired
    private PaymentPositionRepository paymentPositionRepository;
    @PersistenceUnit
	private EntityManagerFactory emf;
    
    public HistoricizationScheduler(PaymentPositionRepository paymentPositionRepository) {
		super();
		this.paymentPositionRepository = paymentPositionRepository;
	}
  
    @Scheduled(cron = "${cron.job.schedule.history.trigger}")
    @SchedulerLock(name = "HistoricizationScheduler_manageDebtPositionsToHistoricize", lockAtMostFor = "${cron.job.schedule.history.shedlock.lockatmostfor}", 
    lockAtLeastFor = "${cron.job.schedule.history.shedlock.lockatleastfor}")
    @Async
    @Transactional
    public void manageDebtPositionsToHistoricize() throws JsonProcessingException, TableServiceException {
    	log.info(String.format(LOG_BASE_HEADER_INFO, CRON_JOB, METHOD, "Running at " + DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss").format(LocalDateTime.now())));
    	EntityManager em = this.getEntityManager();
    	LocalDateTime ldt = LocalDateTime.now().minusDays(extractionInterval);
    	List<PaymentPosition> ppList;
    	if (paginationMode) {
    		long countResult = em.createQuery(countExtractionQuery, Long.class).setParameter(1,ldt).getSingleResult(); 
    		int numOfPages = (int)Math.ceil((float)countResult/pageSize);
    		for (int pageNumber=0; pageNumber<numOfPages; pageNumber++) {
    			ppList = em.createQuery(extractionQuery, PaymentPosition.class).setParameter(1,ldt).setFirstResult((pageNumber) * pageSize).setMaxResults(pageSize).getResultList();
    			log.debug(String.format(LOG_BASE_HEADER_INFO, CRON_JOB, METHOD, 
    					"Paginated historical extraction info: Found n. "+countResult+" debt positions to archive splitted on n. "+numOfPages+" of pages. " + 
    					System.lineSeparator() +
    			        "Page number "+pageNumber+" has been extracted and contains "+ppList.size()+" occurrences"));
    			this.archivesDebtPositions(ppList);
    			// archived debt positions are removed 
    	    	paymentPositionRepository.deleteAll(ppList);
    	    	log.info(String.format(LOG_BASE_HEADER_INFO, CRON_JOB, METHOD, "deleted n. "+ppList.size()+" archived debt positions"));
    		}
    	} else {
    		ppList = em.createQuery(extractionQuery, PaymentPosition.class).setParameter(1,ldt).getResultList(); 
    		log.debug(String.format(LOG_BASE_HEADER_INFO, CRON_JOB, METHOD, "Total entries historical extraction info: Number of extracted rows to historicize: " + ppList.size()));
    		this.archivesDebtPositions(ppList);
    		// archived debt positions are removed 
        	paymentPositionRepository.deleteAll(ppList);
        	log.info(String.format(LOG_BASE_HEADER_INFO, CRON_JOB, METHOD, "deleted n. "+ppList.size()+" archived debt positions"));
    	} 
    	log.info(String.format(LOG_BASE_HEADER_INFO, CRON_JOB, METHOD, "Finished at " + DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss").format(LocalDateTime.now())));
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

	public void upsertPOTable(String organizationFiscalCode, PaymentPosition pp, PaymentOption po) {
	    TableClient tableClient = this.getTableClient(archiveStorageConnection, archiveStoragePOTable);
		TableEntity tableEntity = new TableEntity(organizationFiscalCode, po.getIuv());
		try {
			Map<String, Object> properties = new HashMap<>();
			properties.put("PaymentDate", po.getPaymentDate());
			properties.put("IUPD", pp.getIupd());
			tableEntity.setProperties(properties);
			tableClient.createEntity(tableEntity);
		} catch (TableServiceException e) {
			if (e.getValue().getErrorCode() == TableErrorCode.ENTITY_ALREADY_EXISTS) {
				log.warn(String.format(LOG_BASE_HEADER_INFO, CRON_JOB, "saveToPOTable",
						TableErrorCode.ENTITY_ALREADY_EXISTS + " managed error while storing the table information [organizationFiscalCode="+organizationFiscalCode+", iuv="+po.getIuv()+"]"), e);
				tableClient.updateEntity(tableEntity);
			} else {
				log.error(String.format(LOG_BASE_HEADER_INFO, CRON_JOB, "saveToPOTable",
						"error while storing the table information [organizationFiscalCode="+organizationFiscalCode+", iuv="+po.getIuv()+"]"), e);
				throw e;
			}
		}
	}

	public void upsertPPTable(String organizationFiscalCode, PaymentPosition pp, ObjectMapper objectMapper) throws JsonProcessingException {
		TableClient tableClient = this.getTableClient(archiveStorageConnection, archiveStoragePPTable);
		TableEntity tableEntity = new TableEntity(organizationFiscalCode, pp.getIupd());
		try {
			Map<String, Object> properties = new HashMap<>();
			properties.put("PaymentPosition", objectMapper.writeValueAsString(pp));
			tableEntity.setProperties(properties);
			tableClient.createEntity(tableEntity);
		} catch (TableServiceException e) {
			if (e.getValue().getErrorCode() == TableErrorCode.ENTITY_ALREADY_EXISTS) {
				log.warn(String.format(LOG_BASE_HEADER_INFO, CRON_JOB, "saveToPPTable",
						TableErrorCode.ENTITY_ALREADY_EXISTS + " managed error while storing the table information [organizationFiscalCode="+organizationFiscalCode+", iupd="+pp.getIupd()+"]"), e);
				tableClient.updateEntity(tableEntity);
			} else {
				log.error(String.format(LOG_BASE_HEADER_INFO, CRON_JOB, "saveToPPTable",
						"error while storing the table information [organizationFiscalCode="+organizationFiscalCode+", iupd="+pp.getIupd()+"]"), e);
				throw e;
			}
		}
	}
	
	private void archivesDebtPositions(List<PaymentPosition> ppList) throws JsonProcessingException {
		Map<String, List<PaymentPosition>> ppListByOrganizationFiscalCode = ppList.stream()
                .collect(Collectors.groupingBy(p -> p.getOrganizationFiscalCode(), Collectors.mapping((PaymentPosition p) -> p, Collectors.toList())));
    	
    	ObjectMapper objectMapper = new ObjectMapper();
    	objectMapper.registerModule(new JavaTimeModule());
    	
    	for (Entry<String, List<PaymentPosition>> entry : ppListByOrganizationFiscalCode.entrySet()) {
            List<PaymentPosition> organizationPpList = ppListByOrganizationFiscalCode.get(entry.getKey());
            for (PaymentPosition pp: organizationPpList) {
            	pp.getPaymentOption().forEach(po -> 
            		// write on azure table storage to persist the PO debt position info
                	this.upsertPOTable(entry.getKey(), pp, po)
            	);
            	// write on azure table storage to persist the PP debt position info and json
            	this.upsertPPTable(entry.getKey(), pp, objectMapper);
            }
            log.debug(String.format(LOG_BASE_HEADER_INFO, CRON_JOB, "archivesDebtPositions", "historicized n. "+organizationPpList.size()+" debt positions for the organization fiscal code: " +entry.getKey()));
        }
	}
}