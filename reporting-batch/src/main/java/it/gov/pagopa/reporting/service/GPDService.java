package it.gov.pagopa.reporting.service;

import it.gov.pagopa.reporting.models.Organizations;

import javax.ws.rs.client.ClientBuilder;
import javax.ws.rs.core.MediaType;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;

public class GPDService {

    private String gpdHost = System.getenv("GPD_HOST");
    private final String gpdOrganizationsService = "/organizations";

    private static GPDService instance = null;

    private GPDService() {}

    public static GPDService getInstance() {
        if (instance == null) {
            instance = new GPDService();
        }
        return instance;
    }

    public Organizations getOrganizations(LocalDate since) {
        String sinceDate = since.format(DateTimeFormatter.ofPattern("dd-MMM-yyyy"));

        return  ClientBuilder.newClient()
                .target(gpdHost + gpdOrganizationsService)
                .queryParam("since", sinceDate)
                .request()
                .accept(MediaType.APPLICATION_JSON)
                .get(Organizations.class);
    }

}
