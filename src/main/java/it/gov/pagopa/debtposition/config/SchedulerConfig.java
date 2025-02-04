package it.gov.pagopa.debtposition.config;

import javax.sql.DataSource;
import net.javacrumbs.shedlock.core.LockProvider;
import net.javacrumbs.shedlock.provider.jdbctemplate.JdbcTemplateLockProvider;
import net.javacrumbs.shedlock.spring.annotation.EnableSchedulerLock;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.scheduling.annotation.EnableAsync;
import org.springframework.scheduling.annotation.EnableScheduling;

@Configuration
@EnableScheduling
@EnableAsync
@EnableSchedulerLock(
    defaultLockAtMostFor = "${cron.job.schedule.history.shedlock.defaultlockatmostfor}")
public class SchedulerConfig {
  @Value("${spring.jpa.properties.hibernate.default_schema:apd}")
  private String defaultSchema;

  @Bean
  public LockProvider lockProvider(DataSource dataSource) {
    return new JdbcTemplateLockProvider(dataSource, defaultSchema + ".shedlock");
  }
}
