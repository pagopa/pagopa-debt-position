package it.gov.pagopa.debtposition.scheduler;

import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
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

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;

import it.gov.pagopa.debtposition.entity.PaymentPosition;
import it.gov.pagopa.debtposition.repository.PaymentPositionRepository;
import lombok.extern.slf4j.Slf4j;

@Component
@Slf4j
@ConditionalOnProperty(name = "cron.job.schedule.historicization.enabled", matchIfMissing = true)
public class HistoricizationScheduler {

    private static final String LOG_BASE_HEADER_INFO = "[OperationType: %s] - [ClassMethod: %s] - [MethodParamsToLog: %s]";
    private static final String CRON_JOB = "CRON JOB";
    private static final String METHOD = "manageDebtPositionsToHistoricize";
    private Thread threadOfExecution;
    
    @Value("${cron.job.schedule.extraction.history.query:SELECT pp FROM PaymentPosition pp WHERE pp.status IN ('PAID', 'REPORTED', 'INVALID', 'EXPIRED') AND pp.lastUpdatedDate < ?1}")
    private String extractionQuery;
    @Value("${cron.job.schedule.extraction.history.query.interval:1}")
    private byte extractionInterval;
    
    @Autowired
    private PaymentPositionRepository paymentPositionRepository;
    @PersistenceUnit
	private EntityManagerFactory emf;
    
    
    @Scheduled(cron = "${cron.job.schedule.expression.historicization.debt.positions}")
    @Async
    @Transactional
    public void manageDebtPositionsToHistoricize() throws JsonProcessingException {
    	log.info(String.format(LOG_BASE_HEADER_INFO, CRON_JOB, METHOD, "Running at " + DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss").format(LocalDateTime.now())));
    	EntityManager em = emf.createEntityManager();
    	LocalDateTime ldt = LocalDateTime.now().minusYears(extractionInterval);
    	List<PaymentPosition> ppList = em.createQuery(extractionQuery, PaymentPosition.class).setParameter(1,ldt).getResultList(); 
    	log.info(String.format(LOG_BASE_HEADER_INFO, CRON_JOB, METHOD, "Number of extracted rows to historicize: " + ppList.size()));
    	
    	Map<String, List<PaymentPosition>> ppListByOrganizationFiscalCode = ppList.stream()
                .collect(Collectors.groupingBy(p -> p.getOrganizationFiscalCode(), Collectors.mapping((PaymentPosition p) -> p, Collectors.toList())));
    	
    	ObjectMapper objectMapper = new ObjectMapper();
    	objectMapper.registerModule(new JavaTimeModule());
    	
    	for (Entry<String, List<PaymentPosition>> entry : ppListByOrganizationFiscalCode.entrySet()) {
            List<PaymentPosition> organizationPpList = ppListByOrganizationFiscalCode.get(entry.getKey());
            for (PaymentPosition pp: organizationPpList) {
            	pp.getPaymentOption().forEach(po -> {
            		// TODO write on azure table storage 'table1' to persist the debt position info
                	//System.out.print("[");
            	    //System.out.print(entry.getKey() +","+po.getIuv()+","+po.getPaymentDate()+","+pp.getIupd());
            	    //System.out.print("]");
            	});
            	// TODO write on azure table storage 'table2' to persist the debt position json
            	//System.out.print("(");
        	    //System.out.print(entry.getKey() +","+pp.getIupd()+","+objectMapper.writeValueAsString(pp));
        	    //System.out.print(")");
            }
            log.info(String.format(LOG_BASE_HEADER_INFO, CRON_JOB, METHOD, "historicized n. "+organizationPpList.size()+" debt positions for the organization fiscal code: " +entry.getKey()));
        }
    	
    	// historicized debt positions are removed 
    	//paymentPositionRepository.deleteAll(ppList);
    	log.info(String.format(LOG_BASE_HEADER_INFO, CRON_JOB, METHOD, "deleted historicized debt positions"));
    }

    public Thread getThreadOfExecution() {
        return this.threadOfExecution;
    }
}
