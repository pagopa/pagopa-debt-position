package it.gov.pagopa.debtposition.config.datasource.odp;

import lombok.Getter;
import lombok.Setter;
import org.springframework.boot.context.properties.ConfigurationProperties;

@Getter
@Setter
@ConfigurationProperties(prefix = "odp.datasource")
public class OdpDatasourceProperties {
    private String url;
    private String username;
    private String password;
    private String schema;
}