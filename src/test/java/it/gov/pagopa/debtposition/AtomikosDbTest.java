package it.gov.pagopa.debtposition;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.transaction.jta.JtaTransactionManager;

import jakarta.persistence.EntityManager;

@SpringBootTest(
  webEnvironment = SpringBootTest.WebEnvironment.MOCK,
  classes = it.gov.pagopa.debtposition.DebtPositionApplication.class
)
@ActiveProfiles({"test","atomikos-smoke"})
class AtomikosDbTest {

  @Autowired(required = false)
  JtaTransactionManager jta; // JTA Transaction Manager
  
  @Autowired
  EntityManager em; // JPA EntityManager

  @Autowired
  JdbcTemplate jdbc;
  
  @Autowired org.springframework.transaction.PlatformTransactionManager tm;

  @Test
  void jtaPresent() {
    assertThat(jta).as("JtaTransactionManager should be present").isNotNull();
  }
  
  // Check that the transaction manager is indeed a JTA one
  @Test
  void transactionManagerIsJta() {
    assertThat(tm).isInstanceOf(JtaTransactionManager.class);
  }

  @Test
  @Transactional 
  void simpleJTA_JDBC_Select() {
    Integer one = jdbc.queryForObject("SELECT 1", Integer.class);
    assertThat(one).isEqualTo(1);
  }
  
  @Test
  @Transactional
  void simpleJTA_JPA_EM_Select() {
    Object res = em.createNativeQuery("SELECT 1").getSingleResult();
    int one = ((Number) res).intValue();
    assertThat(one).isEqualTo(1);
  }
}
