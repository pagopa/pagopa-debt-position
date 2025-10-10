package it.gov.pagopa.debtposition.config.datasource.apd;

import lombok.Getter;
import lombok.Setter;
import org.springframework.boot.context.properties.ConfigurationProperties;

@Getter
@Setter
@ConfigurationProperties(prefix = "apd.datasource")
public class ApdDatasourceProperties {
    private String url;
    private String username;
    private String password;
    private String schema;
}