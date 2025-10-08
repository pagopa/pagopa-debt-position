package it.gov.pagopa.debtposition.config.datasource.odp;

import com.atomikos.jdbc.AtomikosDataSourceBean;
import it.gov.pagopa.debtposition.config.datasource.AtomikosJtaPlatform;
import org.postgresql.xa.PGXADataSource;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.DependsOn;
import org.springframework.data.jpa.repository.config.EnableJpaRepositories;
import org.springframework.orm.jpa.JpaVendorAdapter;
import org.springframework.orm.jpa.LocalContainerEntityManagerFactoryBean;

import java.util.HashMap;

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
    public AtomikosDataSourceBean odpDataSource() {
        PGXADataSource pgxa = new PGXADataSource();
        pgxa.setUrl(odpDatasourceProperties.getUrl());
        pgxa.setUser(odpDatasourceProperties.getUsername());
        pgxa.setPassword(odpDatasourceProperties.getPassword());
        pgxa.setCurrentSchema(odpDatasourceProperties.getSchema());

        AtomikosDataSourceBean dataSource = new AtomikosDataSourceBean();
        // Configure the data source
        dataSource.setUniqueResourceName(odpDatasourceProperties.getSchema());
        dataSource.setXaDataSource(pgxa);

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