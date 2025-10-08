package it.gov.pagopa.debtposition.config.datasource.apd;

import com.atomikos.jdbc.AtomikosDataSourceBean;
import it.gov.pagopa.debtposition.config.datasource.AtomikosJtaPlatform;
import org.postgresql.xa.PGXADataSource;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.DependsOn;
import org.springframework.context.annotation.Primary;
import org.springframework.data.jpa.repository.config.EnableJpaRepositories;
import org.springframework.orm.jpa.JpaVendorAdapter;
import org.springframework.orm.jpa.LocalContainerEntityManagerFactoryBean;

import java.util.HashMap;

@Configuration
@DependsOn("transactionManager")
@EnableJpaRepositories(basePackages = "it.gov.pagopa.debtposition.repository.apd",
        entityManagerFactoryRef = "apdEntityManager", transactionManagerRef = "transactionManager")
@EnableConfigurationProperties(ApdDatasourceProperties.class)
public class ApdConfig {
    private final JpaVendorAdapter jpaVendorAdapter;
    private final ApdDatasourceProperties apdDatasourceProperties;

    @Autowired
    public ApdConfig(JpaVendorAdapter jpaVendorAdapter, ApdDatasourceProperties apdDatasourceProperties) {
        this.jpaVendorAdapter = jpaVendorAdapter;
        this.apdDatasourceProperties = apdDatasourceProperties;
    }

    @Primary
    @Bean(name = "apdDataSource", initMethod = "init", destroyMethod = "close")
    public AtomikosDataSourceBean apdDataSource() {
        PGXADataSource pgxa = new PGXADataSource();
        pgxa.setUrl(apdDatasourceProperties.getUrl());
        pgxa.setUser(apdDatasourceProperties.getUsername());
        pgxa.setPassword(apdDatasourceProperties.getPassword());
        pgxa.setCurrentSchema(apdDatasourceProperties.getSchema());

        AtomikosDataSourceBean dataSource = new AtomikosDataSourceBean();
        // Configure the data source
        dataSource.setUniqueResourceName(apdDatasourceProperties.getSchema());
        dataSource.setXaDataSource(pgxa);

        return dataSource;
    }

    @Primary
    @Bean(name = "apdEntityManager")
    @DependsOn("transactionManager")
    public LocalContainerEntityManagerFactoryBean apdEntityManager() {
        HashMap<String, Object> properties = new HashMap<>();
        properties.put("hibernate.transaction.jta.platform", AtomikosJtaPlatform.class.getName());

        LocalContainerEntityManagerFactoryBean entityManager = new LocalContainerEntityManagerFactoryBean();
        entityManager.setJtaDataSource(apdDataSource());
        entityManager.setJpaVendorAdapter(jpaVendorAdapter);
        entityManager.setPackagesToScan("it.gov.pagopa.debtposition.entity.apd");
        entityManager.setPersistenceUnitName("apdPersistenceUnit");
        entityManager.setJpaPropertyMap(properties);
        return entityManager;
    }
}