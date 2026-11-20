package it.gov.pagopa.debtposition.config.logging;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import ch.qos.logback.classic.Level;
import ch.qos.logback.classic.Logger;
import ch.qos.logback.classic.spi.ILoggingEvent;
import ch.qos.logback.core.read.ListAppender;
import feign.Client;
import feign.Request;
import feign.Response;

import it.gov.pagopa.debtposition.config.LoggingAspect;
import java.io.IOException;
import java.net.SocketTimeoutException;
import java.nio.charset.StandardCharsets;
import java.util.Collections;
import org.aspectj.lang.JoinPoint;
import org.aspectj.lang.Signature;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.slf4j.LoggerFactory;
import org.slf4j.MDC;

class FailureLoggingTest {

  private final ListAppender<ILoggingEvent> appender = new ListAppender<>();
  private Logger logger;

  @BeforeEach
  void attachAppender() {
    logger = (Logger) LoggerFactory.getLogger(FailureLogger.class);
    appender.start();
    logger.addAppender(appender);
  }

  @AfterEach
  void detachAppender() {
    logger.detachAppender(appender);
    MDC.clear();
  }

  private static Request request() {
    return Request.create(
        Request.HttpMethod.POST,
        "http://node.local/nodo/checkPosition",
        Collections.emptyMap(),
        null,
        StandardCharsets.UTF_8,
        null);
  }

  private static Response response(int status, Request request) {
    return Response.builder()
        .status(status)
        .reason("reason")
        .request(request)
        .headers(Collections.emptyMap())
        .build();
  }

  @Test
  void successfulFeignCallIsNotLogged() throws IOException {
    Request request = request();
    Client delegate = (req, options) -> response(200, req);
    Client client = new FeignFailureLoggingCapability("node").enrich(delegate);

    Response result = client.execute(request, new Request.Options());

    assertThat(result.status()).isEqualTo(200);
    assertThat(appender.list).isEmpty();
  }

  @Test
  void feignHttpErrorIsLoggedAtErrorWithStatus() throws IOException {
    Request request = request();
    Client delegate = (req, options) -> response(503, req);
    Client client = new FeignFailureLoggingCapability("node").enrich(delegate);

    Response result = client.execute(request, new Request.Options());

    assertThat(result.status()).isEqualTo(503);
    assertThat(appender.list).hasSize(1);
    ILoggingEvent event = appender.list.get(0);
    assertThat(event.getLevel()).isEqualTo(Level.ERROR);
    assertThat(event.getMessage()).isEqualTo(FeignFailureLoggingCapability.MESSAGE);
    assertThat(event.getMDCPropertyMap())
        .containsEntry(FailureLogger.EVENT_ACTION, "HTTP-node-POST")
        .containsEntry(FailureLogger.EVENT_OUTCOME, FailureLogger.OUTCOME_FAILURE)
        .containsEntry(FailureLogger.DEPENDENCY, "node")
        .containsEntry(FailureLogger.HTTP_CODE, "503");
  }

  @Test
  void feignIoErrorIsLoggedAndRethrown() {
    Request request = request();
    IOException ioException = new IOException("connection refused");
    Client delegate =
        (req, options) -> {
          throw ioException;
        };
    Client client = new FeignFailureLoggingCapability("send").enrich(delegate);

    assertThatThrownBy(() -> client.execute(request, new Request.Options()))
        .isSameAs(ioException);

    assertThat(appender.list).hasSize(1);
    ILoggingEvent event = appender.list.get(0);
    assertThat(event.getLevel()).isEqualTo(Level.ERROR);
    assertThat(event.getThrowableProxy().getMessage()).isEqualTo("connection refused");
    assertThat(event.getMDCPropertyMap())
        .containsEntry(FailureLogger.EVENT_ACTION, "HTTP-send-POST")
        .doesNotContainKey(FailureLogger.HTTP_CODE);
  }

  @Test
  void feignTimeoutIsLoggedAsFailure() {
    Request request = request();
    Client delegate =
        (req, options) -> {
          throw new SocketTimeoutException("read timed out");
        };
    Client client = new FeignFailureLoggingCapability("node").enrich(delegate);

    assertThatThrownBy(() -> client.execute(request, new Request.Options()))
        .isInstanceOf(SocketTimeoutException.class);

    assertThat(appender.list).hasSize(1);
    assertThat(appender.list.get(0).getLevel()).isEqualTo(Level.ERROR);
  }

  @Test
  void repositoryFailureIsLoggedWithDatabaseDependency() {
    JoinPoint joinPoint = mock(JoinPoint.class);
    Signature signature = mock(Signature.class);
    when(joinPoint.getSignature()).thenReturn(signature);
    when(signature.getDeclaringType()).thenReturn(PaymentOptionRepositoryStub.class);
    when(signature.getName()).thenReturn("findById");
    RuntimeException cause = new RuntimeException("connection refused");

    new LoggingAspect().logRepositoryFailure(joinPoint, cause);

    assertThat(appender.list).hasSize(1);
    ILoggingEvent event = appender.list.get(0);
    assertThat(event.getLevel()).isEqualTo(Level.ERROR);
    assertThat(event.getMessage()).isEqualTo("Database operation failed");
    assertThat(event.getMDCPropertyMap())
        .containsEntry(FailureLogger.EVENT_ACTION, "DB-PaymentOptionRepositoryStub-findById")
        .containsEntry(FailureLogger.DEPENDENCY, "postgres");
    assertThat(event.getThrowableProxy().getMessage()).isEqualTo("connection refused");
  }

  @Test
  void failureLogRemovesItsMdcKeys() {
    FailureLogger.log("msg", "action", "dep", 500, null);

    assertThat(MDC.get(FailureLogger.EVENT_ACTION)).isNull();
    assertThat(MDC.get(FailureLogger.DEPENDENCY)).isNull();
    assertThat(MDC.get(FailureLogger.HTTP_CODE)).isNull();
    assertThat(appender.list).hasSize(1);
  }

  /** Named type used to derive the DB event action in the repository test. */
  private interface PaymentOptionRepositoryStub {}
}
