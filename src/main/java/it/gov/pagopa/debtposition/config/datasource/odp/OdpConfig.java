package it.gov.pagopa.debtposition.config.datasource.odp;

import com.atomikos.jdbc.AtomikosDataSourceBean;
import it.gov.pagopa.debtposition.config.datasource.AtomikosJtaPlatform;
import org.postgresql.xa.PGXADataSource;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.DependsOn;
import org.springframework.data.jpa.repository.config.EnableJpaRepositories;
import org.springframework.orm.jpa.JpaVendorAdapter;
import org.springframework.orm.jpa.LocalContainerEntityManagerFactoryBean;

import java.util.HashMap;
import java.util.UUID;

@Configuration
@DependsOn("transactionManager")
@EnableJpaRepositories(basePackages = "it.gov.pagopa.debtposition.repository.odp",
        entityManagerFactoryRef = "odpEntityManager", transactionManagerRef = "transactionManager")
@EnableConfigurationProperties(OdpDatasourceProperties.class)
public class OdpConfig {
    private final JpaVendorAdapter jpaVendorAdapter;
    private final OdpDatasourceProperties odpDatasourceProperties;

    @Autowired
    public OdpConfig(JpaVendorAdapter jpaVendorAdapter, OdpDatasourceProperties odpDatasourceProperties) {
        this.jpaVendorAdapter = jpaVendorAdapter;
        this.odpDatasourceProperties = odpDatasourceProperties;
    }

    @Bean(name = "odpDataSource", initMethod = "init", destroyMethod = "close")
    @ConditionalOnMissingBean(name = "odpDataSource")
    public AtomikosDataSourceBean odpDataSource() {
        String url = odpDatasourceProperties.getUrl();
        String user = odpDatasourceProperties.getUsername();
        String password = odpDatasourceProperties.getPassword();

        AtomikosDataSourceBean dataSource = new AtomikosDataSourceBean();
        // Configure the data source
        if (url != null && url.startsWith("jdbc:h2")) {
            // H2 for testing
            java.util.Properties xaProps = new java.util.Properties();
            xaProps.setProperty("URL", url);
            xaProps.setProperty("user", user);
            xaProps.setProperty("password", password);
            dataSource.setXaDataSourceClassName("org.h2.jdbcx.JdbcDataSource");
            dataSource.setXaProperties(xaProps);
            dataSource.setUniqueResourceName(odpDatasourceProperties.getSchema() + UUID.randomUUID());
        } else {
            // Postgres for production
            PGXADataSource pgxa = new PGXADataSource();
            pgxa.setUrl(odpDatasourceProperties.getUrl());
            pgxa.setUser(odpDatasourceProperties.getUsername());
            pgxa.setPassword(odpDatasourceProperties.getPassword());
            pgxa.setCurrentSchema(odpDatasourceProperties.getSchema());
            dataSource.setXaDataSource(pgxa);
            dataSource.setUniqueResourceName(odpDatasourceProperties.getSchema());
        }

        return dataSource;
    }

    @Bean(name = "odpEntityManager")
    public LocalContainerEntityManagerFactoryBean odpEntityManager() {
        HashMap<String, Object> properties = new HashMap<>();
        properties.put("hibernate.transaction.jta.platform", AtomikosJtaPlatform.class.getName());

        LocalContainerEntityManagerFactoryBean entityManager = new LocalContainerEntityManagerFactoryBean();
        entityManager.setJtaDataSource(odpDataSource());
        entityManager.setJpaVendorAdapter(jpaVendorAdapter);
        entityManager.setPackagesToScan("it.gov.pagopa.debtposition.entity.odp");
        entityManager.setPersistenceUnitName("odpPersistenceUnit");
        entityManager.setJpaPropertyMap(properties);
        return entityManager;
    }
}