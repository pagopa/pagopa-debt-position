package it.gov.pagopa.debtposition.config;

import com.atomikos.icatch.jta.UserTransactionManager;
import com.atomikos.icatch.jta.UserTransactionImp;
import com.atomikos.jdbc.AtomikosDataSourceBean;
import jakarta.transaction.UserTransaction;
import jakarta.transaction.TransactionManager;

import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.context.annotation.*;
import org.springframework.transaction.PlatformTransactionManager;
import org.springframework.transaction.annotation.EnableTransactionManagement;
import org.springframework.transaction.jta.JtaTransactionManager;

import javax.sql.DataSource;

@Profile("atomikos-smoke")
@Configuration
@EnableTransactionManagement
public class AtomikosConfig {

	@Bean(initMethod = "init", destroyMethod = "close")
	AtomikosDataSourceBean xaDataSource(org.springframework.core.env.Environment env) {
	  String url  = env.getProperty("spring.datasource.url",
	                  System.getProperty("DB_URL", "jdbc:postgresql://localhost:5432/test"));
	  String user = env.getProperty("spring.datasource.username",
	                  System.getProperty("DB_USER", "postgres"));
	  String pwd  = env.getProperty("spring.datasource.password",
	                  System.getProperty("DB_PASSWORD", "postgres"));

	  AtomikosDataSourceBean ds = new AtomikosDataSourceBean();
	  ds.setUniqueResourceName("gpd-xa");
	  ds.setMinPoolSize(1);
	  ds.setMaxPoolSize(5);

	  if (url != null && url.startsWith("jdbc:h2")) {
	    // H2 for testing
	    java.util.Properties xaProps = new java.util.Properties();
	    xaProps.setProperty("URL", url);
	    xaProps.setProperty("user", user);
	    xaProps.setProperty("password", pwd);
	    ds.setXaDataSourceClassName("org.h2.jdbcx.JdbcDataSource");
	    ds.setXaProperties(xaProps);
	  } else {
	    // Postgres for dev
	    org.postgresql.xa.PGXADataSource pgxa = new org.postgresql.xa.PGXADataSource();
	    pgxa.setUrl(url);
	    pgxa.setUser(user);
	    pgxa.setPassword(pwd);
	    ds.setXaDataSource(pgxa);
	  }
	  return ds;
	}

  @Bean
  @Primary
  DataSource dataSource(AtomikosDataSourceBean xa) {
    return xa;
  }

  // JTA core
  @Bean(initMethod = "init", destroyMethod = "close")
  UserTransactionManager atomikosTransactionManager() {
    UserTransactionManager utm = new UserTransactionManager();
    utm.setForceShutdown(true);
    return utm;
  }

  @Bean
  @Primary
  UserTransaction userTransaction() throws Exception {
    UserTransactionImp ut = new UserTransactionImp();
    ut.setTransactionTimeout(300);
    return ut;
  }

  @Bean
  PlatformTransactionManager transactionManager(@Qualifier("userTransaction") UserTransaction ut, TransactionManager tm) {
    return new JtaTransactionManager(ut, tm);
  }
}