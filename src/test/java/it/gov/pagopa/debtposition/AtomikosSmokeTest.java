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
class AtomikosSmokeTest {

  @Autowired(required = false)
  JtaTransactionManager jta;
  
  @Autowired
  EntityManager em;

  @Autowired
  JdbcTemplate jdbc;

  @Test
  void jtaPresent() {
    assertThat(jta).as("JtaTransactionManager should be present").isNotNull();
  }

  @Test
  @Transactional 
  void simpleJTASelect() {
    Integer one = jdbc.queryForObject("SELECT 1", Integer.class);
    assertThat(one).isEqualTo(1);
  }
  
  @Test
  @Transactional
  void simpleNativeSelect() {
    Object res = em.createNativeQuery("SELECT 1").getSingleResult();
    int one = ((Number) res).intValue();
    assertThat(one).isEqualTo(1);
  }
}
