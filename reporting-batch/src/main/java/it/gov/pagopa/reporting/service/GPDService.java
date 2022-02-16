package it.gov.pagopa.reporting.service;

import org.apache.http.client.utils.URIBuilder;

import javax.ws.rs.client.ClientBuilder;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import java.io.IOException;
import java.net.*;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.*;

public class GPDService {

    private String gpdHost = System.getenv("GPD_HOST");
    private String gpdOrganizationsService = System.getenv("GPD_ORGANIZATIONS_SERVICE");

    private static GPDService instance = null;

    private GPDService() {}

    public static GPDService getInstance() {
        if (instance == null) {
            instance = new GPDService();
        }
        return instance;
    }

    public List getOrganizations(LocalDate since) {
        String sinceDate = since.format(DateTimeFormatter.ofPattern("dd-MMM-yyyy"));

        Response response = ClientBuilder.newClient()
                .target(gpdHost + gpdOrganizationsService)
                .queryParam("since", sinceDate)
                .request()
                .accept(MediaType.APPLICATION_JSON)
                .get();
        System.out.println(response);

        /*
        try {

            URIBuilder builder = new URIBuilder(gpdHost + gpdOrganizationsService);
            builder.addParameter("since", sinceDate);

            HttpRequest request = HttpRequest.newBuilder()
                    .uri(builder.build())
                    .GET()
                    .build();

            HttpClient client = HttpClient.newHttpClient();
            String data = client.send(request, HttpResponse.BodyHandlers.ofString()).body();

        } catch (URISyntaxException | InterruptedException | IOException e) {
            e.printStackTrace();
        }
         */


        return new ArrayList();
    }
}
