package it.gov.pagopa.reporting.service;

import it.gov.pagopa.reporting.models.Organizations;

import javax.ws.rs.client.ClientBuilder;
import javax.ws.rs.core.MediaType;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;

public class GPDService {

    private String gpdHost = System.getenv("GPD_HOST");
    private static final String GDP_ORGANIZATIONS_SERVICE = "/organizations";

    private static GPDService instance = null;

    private GPDService() {}

    public static GPDService getInstance() {
        if (instance == null) {
            instance = new GPDService();
        }
        return instance;
    }

    public Organizations getOrganizations(LocalDate since) {
        String sinceDate = since.format(DateTimeFormatter.ofPattern("yyyy-MM-dd"));

        return  ClientBuilder.newClient()
                .target(gpdHost + GDP_ORGANIZATIONS_SERVICE)
                .queryParam("since", sinceDate)
                .request()
                .accept(MediaType.APPLICATION_JSON)
                .get(Organizations.class);
    }
}
