package it.gov.pagopa.debtposition.config.logging;

import feign.Capability;
import feign.Client;
import feign.Request;
import feign.Response;
import java.io.IOException;

/**
 * Logs outbound Feign calls only when they fail: HTTP error statuses (4xx/5xx) and I/O errors
 * such as connection refused or timeouts. Successful calls are not logged.
 */
public class FeignLoggingCapability implements Capability {

  static final String MESSAGE = "Outbound HTTP call failed";

  private final String dependency;

  public FeignLoggingCapability(String dependency) {
    this.dependency = dependency;
  }

  @Override
  public Client enrich(Client client) {
    return new FailureLoggingClient(client, dependency);
  }

  static final class FailureLoggingClient implements Client {

    private final Client delegate;
    private final String dependency;

    FailureLoggingClient(Client delegate, String dependency) {
      this.delegate = delegate;
      this.dependency = dependency;
    }

    @Override
    public Response execute(Request request, Request.Options options) throws IOException {
      String eventAction = "HTTP-" + dependency + "-" + request.httpMethod();
      Response response;
      try {
        response = delegate.execute(request, options);
      } catch (IOException e) {
        FailureLogger.log(MESSAGE, eventAction, dependency, null, e);
        throw e;
      }
      if (response.status() >= 400) {
        FailureLogger.log(MESSAGE, eventAction, dependency, response.status(), null);
      }
      return response;
    }
  }
}
