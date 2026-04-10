package it.gov.pagopa.debtposition.scheduler;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import it.gov.pagopa.debtposition.DebtPositionApplication;
import it.gov.pagopa.debtposition.TestUtil;
import it.gov.pagopa.debtposition.dto.PaymentPositionDTO;
import it.gov.pagopa.debtposition.mock.DebtPositionMock;
import it.gov.pagopa.debtposition.model.enumeration.DebtPositionStatus;
import it.gov.pagopa.debtposition.model.enumeration.PaymentOptionStatus;
import net.javacrumbs.shedlock.core.LockProvider;

import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.time.temporal.ChronoUnit;
import java.util.Optional;
import java.util.concurrent.TimeUnit;

import org.awaitility.Awaitility;
import org.hamcrest.core.IsNull;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.result.MockMvcResultMatchers;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.springframework.test.context.DynamicPropertyRegistry;
import org.springframework.test.context.DynamicPropertySource;
import org.springframework.test.context.bean.override.mockito.MockitoBean;
import org.testcontainers.containers.PostgreSQLContainer;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;

@SpringBootTest(classes = DebtPositionApplication.class)
@AutoConfigureMockMvc
@Testcontainers
@Execution(ExecutionMode.SAME_THREAD)
class ExpiredPositionsSchedulerTest {

  @Autowired ExpiredPositionsScheduler expiredPositionsScheduler;

  @Autowired private MockMvc mvc;

  @MockitoBean private LockProvider lockProvider;

  @SuppressWarnings("resource")
  @Container
  static final PostgreSQLContainer<?> postgres =
      new PostgreSQLContainer<>("postgres:15-alpine")
          .withDatabaseName("apd")
          .withUsername("test")
          .withPassword("test")
          .withInitScript("sql/init-postgres-test.sql");

  @DynamicPropertySource
  static void configureProperties(DynamicPropertyRegistry registry) {
    registry.add("spring.datasource.url", postgres::getJdbcUrl);
    registry.add("spring.datasource.username", postgres::getUsername);
    registry.add("spring.datasource.password", postgres::getPassword);
    registry.add("spring.datasource.driver-class-name", postgres::getDriverClassName);
    registry.add("spring.jpa.properties.hibernate.default_schema", () -> "apd");
    registry.add("SCHEMA_NAME", () -> "apd");
    registry.add(
        "spring.jpa.properties.hibernate.dialect",
        () -> "org.hibernate.dialect.PostgreSQLDialect");
    registry.add(
        "spring.jpa.database-platform",
        () -> "org.hibernate.dialect.PostgreSQLDialect");
    registry.add("spring.jpa.hibernate.ddl-auto", () -> "create-drop");
    registry.add("cron.job.schedule.enabled", () -> "true");
    registry.add("cron.job.schedule.expression.valid.status", () -> "0 0 0 1 1 *");
    registry.add("cron.job.schedule.expression.expired.status", () -> "0 0 0 1 1 *");
    registry.add("cron.job.schedule.shedlock.defaultlockatmostfor", () -> "30m");
    registry.add("cron.job.schedule.shedlock.lockatmostfor", () -> "45m");
    registry.add("cron.job.schedule.shedlock.lockatleastfor", () -> "0s");
  }

  @BeforeEach
  void mockShedlock() {
    when(lockProvider.lock(any()))
        .thenReturn(Optional.of(() -> {}));
  }

  @Test
  void manualChangeDebtPositionStatusToValid() throws Exception {

    LocalDateTime now = nowUtc();
    LocalDateTime validityDate = now.plusSeconds(2);
    LocalDateTime dueDate = now.plusSeconds(10);

    PaymentPositionDTO pp =
        prepareScheduledPosition(
            DebtPositionMock.getMock5(), validityDate, dueDate, false);

    // creo una posizione debitoria (con 'validity date')
    mvc.perform(
            post("/organizations/SCHEDULEVALID_12345678901/debtpositions")
                .content(TestUtil.toJson(pp))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated());

    // porto in pubblicata lo stato della posizione debitoria
    mvc.perform(
            post("/organizations/SCHEDULEVALID_12345678901/debtpositions/12345678901IUPDMOCK3/publish")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk());

    // verifico che lo stato sia stato effettivamente aggiornato a PUBLISHED
    mvc.perform(
            get("/organizations/SCHEDULEVALID_12345678901/debtpositions/12345678901IUPDMOCK3")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status")
                .value(DebtPositionStatus.PUBLISHED.toString()))
        .andExpect(MockMvcResultMatchers.jsonPath("$.publishDate").isNotEmpty());

    // introduco un ritardo in modo da far scadere la validity date
    waitUntilAfter(validityDate.plusSeconds(1));

    // lancio il batch per consentire il passaggio di stato
    expiredPositionsScheduler.changeDebtPositionStatusToValid();

    // check status until it becomes VALID
    Awaitility.await()
        .atMost(10, TimeUnit.SECONDS)
        .pollInterval(100, TimeUnit.MILLISECONDS)
        .untilAsserted(
            () ->
                mvc.perform(
                        get("/organizations/SCHEDULEVALID_12345678901/debtpositions/12345678901IUPDMOCK3")
                            .contentType(MediaType.APPLICATION_JSON))
                    .andExpect(status().isOk())
                    .andExpect(content().contentType(MediaType.APPLICATION_JSON))
                    .andExpect(
                        MockMvcResultMatchers.jsonPath("$.status")
                            .value(DebtPositionStatus.VALID.toString())));
  }

  @Test
  void manualChangeDebtPositionStatusToValidAfterDueDate() throws Exception {

    LocalDateTime now = nowUtc();
    LocalDateTime validityDate = now.plusSeconds(2);
    LocalDateTime dueDate = now.plusSeconds(5);

    PaymentPositionDTO pp =
        prepareScheduledPosition(
            DebtPositionMock.getMock5(), validityDate, dueDate, false);

    // creo una posizione debitoria (con 'validity date') senza valorizzare il campo switchToExpired
    // (quindi per default verrà messo a false) -> Lo stato deve rimanere VALID passata la due_date
    mvc.perform(
            post("/organizations/SCHEDULEVALIDAFTERDUEDATE_12345678901/debtpositions")
                .content(TestUtil.toJson(pp))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated());

    // porto in pubblicata lo stato della posizione debitoria
    mvc.perform(
            post("/organizations/SCHEDULEVALIDAFTERDUEDATE_12345678901/debtpositions/12345678901IUPDMOCK3/publish")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk());

    // verifico che lo stato sia stato effettivamente aggiornato a PUBLISHED
    mvc.perform(
            get("/organizations/SCHEDULEVALIDAFTERDUEDATE_12345678901/debtpositions/12345678901IUPDMOCK3")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status")
                .value(DebtPositionStatus.PUBLISHED.toString()))
        .andExpect(MockMvcResultMatchers.jsonPath("$.publishDate").isNotEmpty());

    // introduco un ritardo in modo da far scadere la validity date
    waitUntilAfter(validityDate.plusSeconds(1));

    // lancio il batch per consentire il passaggio di stato
    expiredPositionsScheduler.changeDebtPositionStatusToValid();

    // check status until it becomes VALID
    Awaitility.await()
        .atMost(10, TimeUnit.SECONDS)
        .pollInterval(100, TimeUnit.MILLISECONDS)
        .untilAsserted(
            () ->
                mvc.perform(
                        get("/organizations/SCHEDULEVALIDAFTERDUEDATE_12345678901/debtpositions/12345678901IUPDMOCK3")
                            .contentType(MediaType.APPLICATION_JSON))
                    .andExpect(status().isOk())
                    .andExpect(content().contentType(MediaType.APPLICATION_JSON))
                    .andExpect(
                        MockMvcResultMatchers.jsonPath("$.status")
                            .value(DebtPositionStatus.VALID.toString())));

    // introduco un nuovo ritardo in modo da far scadere la due date
    waitUntilAfter(dueDate.plusSeconds(1));

    // lancio il batch per consentire il passaggio di stato
    expiredPositionsScheduler.changeDebtPositionStatusToExpired();

    // check status until it remains VALID (not pass to EXPIRED because switchToExpired is false)
    Awaitility.await()
        .atMost(10, TimeUnit.SECONDS)
        .pollInterval(100, TimeUnit.MILLISECONDS)
        .untilAsserted(
            () ->
                mvc.perform(
                        get("/organizations/SCHEDULEVALIDAFTERDUEDATE_12345678901/debtpositions/12345678901IUPDMOCK3")
                            .contentType(MediaType.APPLICATION_JSON))
                    .andExpect(status().isOk())
                    .andExpect(content().contentType(MediaType.APPLICATION_JSON))
                    .andExpect(
                        MockMvcResultMatchers.jsonPath("$.status")
                            .value(DebtPositionStatus.VALID.toString())));
  }

  @Test
  void manualChangeDebtPositionStatusToExpiredAfterDueDate() throws Exception {

    LocalDateTime now = nowUtc();
    LocalDateTime validityDate = now.plusSeconds(2);
    LocalDateTime dueDate = now.plusSeconds(5);

    PaymentPositionDTO pp =
        prepareScheduledPosition(
            DebtPositionMock.getMock7(), validityDate, dueDate, true);

    // creo una posizione debitoria (con 'validity date') valorizzando il campo switchToExpired a
    // true -> Lo stato deve passare ad EXPIRED passata la due_date
    mvc.perform(
            post("/organizations/SCHEDULEEXP_12345678901/debtpositions")
                .content(TestUtil.toJson(pp))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated());

    // porto in pubblicata lo stato della posizione debitoria
    mvc.perform(
            post("/organizations/SCHEDULEEXP_12345678901/debtpositions/12345678901IUPDMOCK3/publish")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk());

    // verifico che lo stato sia stato effettivamente aggiornato a PUBLISHED
    mvc.perform(
            get("/organizations/SCHEDULEEXP_12345678901/debtpositions/12345678901IUPDMOCK3")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status")
                .value(DebtPositionStatus.PUBLISHED.toString()))
        .andExpect(MockMvcResultMatchers.jsonPath("$.publishDate").isNotEmpty());

    // introduco un ritardo in modo da far scadere la validity date
    waitUntilAfter(validityDate.plusSeconds(1));

    // lancio il batch per consentire il passaggio di stato
    expiredPositionsScheduler.changeDebtPositionStatusToValid();

    // check status until it becomes VALID
    Awaitility.await()
        .atMost(10, TimeUnit.SECONDS)
        .pollInterval(100, TimeUnit.MILLISECONDS)
        .untilAsserted(
            () ->
                mvc.perform(
                        get("/organizations/SCHEDULEEXP_12345678901/debtpositions/12345678901IUPDMOCK3")
                            .contentType(MediaType.APPLICATION_JSON))
                    .andExpect(status().isOk())
                    .andExpect(content().contentType(MediaType.APPLICATION_JSON))
                    .andExpect(
                        MockMvcResultMatchers.jsonPath("$.status")
                            .value(DebtPositionStatus.VALID.toString())));

    // introduco un nuovo ritardo in modo da far scadere la due date
    waitUntilAfter(dueDate.plusSeconds(1));

    // lancio il batch per consentire il passaggio di stato
    expiredPositionsScheduler.changeDebtPositionStatusToExpired();

    // check status until it becomes EXPIRED
    Awaitility.await()
        .atMost(10, TimeUnit.SECONDS)
        .pollInterval(100, TimeUnit.MILLISECONDS)
        .untilAsserted(
            () ->
                mvc.perform(
                        get("/organizations/SCHEDULEEXP_12345678901/debtpositions/12345678901IUPDMOCK3")
                            .contentType(MediaType.APPLICATION_JSON))
                    .andExpect(status().isOk())
                    .andExpect(content().contentType(MediaType.APPLICATION_JSON))
                    .andExpect(
                        MockMvcResultMatchers.jsonPath("$.status")
                            .value(DebtPositionStatus.EXPIRED.toString())));
  }

  @Test
  void manualChangeDebtPositionStatusToExpiredAndUpdateAllowed() throws Exception {

    LocalDateTime now = nowUtc();
    LocalDateTime validityDate = now.plusSeconds(2);
    LocalDateTime commonDueDate = now.plusSeconds(5);

    PaymentPositionDTO pp7 =
        prepareScheduledPosition(
            DebtPositionMock.getMock7(), validityDate, commonDueDate, true);

    // All POs must be UNPAID + switchable to expired + have a dueDate that will expire soon

    // creo una posizione debitoria (con 'validity date') valorizzando il campo switchToExpired a
    // true -> Lo stato deve passare ad EXPIRED passata la due_date
    mvc.perform(
            post("/organizations/SCHEDULEEXPANDUPD_12345678901/debtpositions")
                .content(TestUtil.toJson(pp7))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated());

    // porto in PUBLISHED lo stato della posizione debitoria
    mvc.perform(
            post("/organizations/SCHEDULEEXPANDUPD_12345678901/debtpositions/12345678901IUPDMOCK3/publish")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk());

    // verifico che lo stato sia stato effettivamente aggiornato a PUBLISHED
    mvc.perform(
            get("/organizations/SCHEDULEEXPANDUPD_12345678901/debtpositions/12345678901IUPDMOCK3")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status")
                .value(DebtPositionStatus.PUBLISHED.toString()))
        .andExpect(MockMvcResultMatchers.jsonPath("$.publishDate").isNotEmpty());

    // introduco un ritardo in modo da far scadere la validity date
    waitUntilAfter(validityDate.plusSeconds(1));

    // lancio il batch per consentire il passaggio di stato
    expiredPositionsScheduler.changeDebtPositionStatusToValid();

    // check status until it becomes VALID
    Awaitility.await()
        .atMost(10, TimeUnit.SECONDS)
        .pollInterval(100, TimeUnit.MILLISECONDS)
        .untilAsserted(
            () ->
                mvc.perform(
                        get("/organizations/SCHEDULEEXPANDUPD_12345678901/debtpositions/12345678901IUPDMOCK3")
                            .contentType(MediaType.APPLICATION_JSON))
                    .andExpect(status().isOk())
                    .andExpect(content().contentType(MediaType.APPLICATION_JSON))
                    .andExpect(
                        MockMvcResultMatchers.jsonPath("$.status")
                            .value(DebtPositionStatus.VALID.toString())));

    // wait until ALL POs have expired due date
    waitUntilAfter(commonDueDate.plusSeconds(1));

    // lancio il batch per consentire il passaggio di stato
    expiredPositionsScheduler.changeDebtPositionStatusToExpired();

    // wait UNTIL the position goes EXPIRED
    Awaitility.await()
        .atMost(10, TimeUnit.SECONDS)
        .pollInterval(100, TimeUnit.MILLISECONDS)
        .untilAsserted(() ->
            mvc.perform(
                    get("/organizations/SCHEDULEEXPANDUPD_12345678901/debtpositions/12345678901IUPDMOCK3")
                        .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk())
                .andExpect(content().contentType(MediaType.APPLICATION_JSON))
                .andExpect(
                    MockMvcResultMatchers.jsonPath("$.status")
                        .value(DebtPositionStatus.EXPIRED.toString()))
        );

    // aggiorno la posizione debitoria (stato atteso DRAFT)
    pp7.setCompanyName("Comune di Napoli");
    pp7.setValidityDate(null);
    pp7.getPaymentOption()
        .get(0)
        .setDueDate(LocalDateTime.now(ZoneOffset.UTC).plus(10, ChronoUnit.SECONDS));
    mvc.perform(
            put("/organizations/SCHEDULEEXPANDUPD_12345678901/debtpositions/12345678901IUPDMOCK3")
                .content(TestUtil.toJson(pp7))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status").value(DebtPositionStatus.DRAFT.toString()));

    // aggiorno la posizione debitoria e ne richiedo la pubblicazione (stato atteso VALID)
    pp7.setCompanyName("Comune di Milano");
    pp7.getPaymentOption()
        .get(0)
        .setDueDate(LocalDateTime.now(ZoneOffset.UTC).plus(7, ChronoUnit.SECONDS));
    mvc.perform(
            put("/organizations/SCHEDULEEXPANDUPD_12345678901/debtpositions/12345678901IUPDMOCK3?toPublish=True")
                .content(TestUtil.toJson(pp7))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status").value(DebtPositionStatus.VALID.toString()));

    // aggiorno la posizione debitoria, sposto nel futuro la validity date e richiedo la
    // pubblicazione (stato atteso PUBLISHED)
    pp7.setCompanyName("Comune di Palermo");

    LocalDateTime publishNow = LocalDateTime.now(ZoneOffset.UTC);
    LocalDateTime futureValidity = publishNow.plus(1, ChronoUnit.DAYS);
    LocalDateTime futureDue = publishNow.plus(2, ChronoUnit.DAYS);

    pp7.setValidityDate(futureValidity);
    pp7.getPaymentOption().forEach(opt -> {
      opt.setStatus(PaymentOptionStatus.PO_UNPAID);
      opt.setSwitchToExpired(true);
      opt.setDueDate(futureDue);
      opt.setValidityDate(futureValidity);
    });

    mvc.perform(
            put("/organizations/SCHEDULEEXPANDUPD_12345678901/debtpositions/12345678901IUPDMOCK3?toPublish=True")
                .content(TestUtil.toJson(pp7))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status")
                .value(DebtPositionStatus.PUBLISHED.toString()));

    // aggiorno la posizione debitoria con un body che non contiene la 'validity date' e ne richiedo
    // la pubblicazione (stato atteso VALID)
    pp7.setCompanyName("Comune di Latina");
    pp7.setValidityDate(null);
    mvc.perform(
            put("/organizations/SCHEDULEEXPANDUPD_12345678901/debtpositions/12345678901IUPDMOCK3?toPublish=True")
                .content(TestUtil.toJson(pp7))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status").value(DebtPositionStatus.VALID.toString()));

    // recupero la posizione debitoria e verifico che lo stato sia in VALID e il dato sia aggiornato
    // all'ultimo valore
    mvc.perform(
            get("/organizations/SCHEDULEEXPANDUPD_12345678901/debtpositions/12345678901IUPDMOCK3")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status").value(DebtPositionStatus.VALID.toString()))
        .andExpect(MockMvcResultMatchers.jsonPath("$.validityDate").value(IsNull.notNullValue()))
        .andExpect(MockMvcResultMatchers.jsonPath("$.companyName").value("Comune di Latina"));
  }
  
  @Test
  void manualChangeDebtPositionStatusToValidShouldNotUpdateBeforeValidityDate() throws Exception {

    LocalDateTime now = nowUtc();
    LocalDateTime validityDate = now.plusDays(1);
    LocalDateTime dueDate = now.plusDays(2);

    PaymentPositionDTO pp =
        prepareScheduledPosition(
            DebtPositionMock.getMock5(), validityDate, dueDate, false);

    pp.setIupd("12345678901IUPDMOCK_VALID_FUTURE");
    pp.getPaymentOption().get(0).setIuv("123456_VALID_FUTURE");

    // create a debt position with a validity date in the future and switchToExpired = false -> The status should remain PUBLISHED even after the batch runs before the validity date
    mvc.perform(
            post("/organizations/SCHEDULEVALIDFUTURE_12345678901/debtpositions")
                .content(TestUtil.toJson(pp))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated());

    // publish the debt position
    mvc.perform(
            post("/organizations/SCHEDULEVALIDFUTURE_12345678901/debtpositions/12345678901IUPDMOCK_VALID_FUTURE/publish")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk());

    // check that the status is PUBLISHED
    mvc.perform(
            get("/organizations/SCHEDULEVALIDFUTURE_12345678901/debtpositions/12345678901IUPDMOCK_VALID_FUTURE")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status")
                .value(DebtPositionStatus.PUBLISHED.toString()))
        .andExpect(MockMvcResultMatchers.jsonPath("$.publishDate").isNotEmpty());

    // run batch before the validity date then the status should remain PUBLISHED
    expiredPositionsScheduler.changeDebtPositionStatusToValid();

    // check that the status is still PUBLISHED
    mvc.perform(
            get("/organizations/SCHEDULEVALIDFUTURE_12345678901/debtpositions/12345678901IUPDMOCK_VALID_FUTURE")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status")
                .value(DebtPositionStatus.PUBLISHED.toString()));
  }
  
  @Test
  void manualChangeDebtPositionStatusToValidShouldUpdateMultiplePositionsInSameRun() throws Exception {

    LocalDateTime now = nowUtc();
    LocalDateTime validityDate = now.plusSeconds(2);
    LocalDateTime dueDate = now.plusSeconds(10);

    PaymentPositionDTO pp1 =
        prepareScheduledPosition(
            DebtPositionMock.getMock5(), validityDate, dueDate, false);

    pp1.setIupd("12345678901IUPDMOCK_BATCH_1");
    pp1.getPaymentOption().get(0).setIuv("123456_BATCH_1");

    PaymentPositionDTO pp2 =
        prepareScheduledPosition(
            DebtPositionMock.getMock5(), validityDate, dueDate, false);

    pp2.setIupd("12345678901IUPDMOCK_BATCH_2");
    pp2.getPaymentOption().get(0).setIuv("123456_BATCH_2");

    // create the first debt position with a validity date in the future and switchToExpired = false
    mvc.perform(
            post("/organizations/SCHEDULEVALIDBATCH_12345678901/debtpositions")
                .content(TestUtil.toJson(pp1))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated());

    // create the second debt position with a validity date in the future and switchToExpired = false
    mvc.perform(
            post("/organizations/SCHEDULEVALIDBATCH_12345678901/debtpositions")
                .content(TestUtil.toJson(pp2))
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated());

    // set the first debt position to PUBLISHED
    mvc.perform(
            post("/organizations/SCHEDULEVALIDBATCH_12345678901/debtpositions/12345678901IUPDMOCK_BATCH_1/publish")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk());

    // set the second debt position to PUBLISHED
    mvc.perform(
            post("/organizations/SCHEDULEVALIDBATCH_12345678901/debtpositions/12345678901IUPDMOCK_BATCH_2/publish")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk());

    // check that the first position is PUBLISHED
    mvc.perform(
            get("/organizations/SCHEDULEVALIDBATCH_12345678901/debtpositions/12345678901IUPDMOCK_BATCH_1")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status")
                .value(DebtPositionStatus.PUBLISHED.toString()));

    // check that the second position is PUBLISHED
    mvc.perform(
            get("/organizations/SCHEDULEVALIDBATCH_12345678901/debtpositions/12345678901IUPDMOCK_BATCH_2")
                .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(
            MockMvcResultMatchers.jsonPath("$.status")
                .value(DebtPositionStatus.PUBLISHED.toString()));

    // set a delay to make the validity date expire for both positions
    waitUntilAfter(validityDate.plusSeconds(1));

    // run the batch to change the status to VALID for both positions in the same run
    expiredPositionsScheduler.changeDebtPositionStatusToValid();

    // check that the first position is now VALID
    Awaitility.await()
        .atMost(10, TimeUnit.SECONDS)
        .pollInterval(100, TimeUnit.MILLISECONDS)
        .untilAsserted(
            () ->
                mvc.perform(
                        get("/organizations/SCHEDULEVALIDBATCH_12345678901/debtpositions/12345678901IUPDMOCK_BATCH_1")
                            .contentType(MediaType.APPLICATION_JSON))
                    .andExpect(status().isOk())
                    .andExpect(content().contentType(MediaType.APPLICATION_JSON))
                    .andExpect(
                        MockMvcResultMatchers.jsonPath("$.status")
                            .value(DebtPositionStatus.VALID.toString())));

    // check that the second position is now VALID
    Awaitility.await()
        .atMost(10, TimeUnit.SECONDS)
        .pollInterval(100, TimeUnit.MILLISECONDS)
        .untilAsserted(
            () ->
                mvc.perform(
                        get("/organizations/SCHEDULEVALIDBATCH_12345678901/debtpositions/12345678901IUPDMOCK_BATCH_2")
                            .contentType(MediaType.APPLICATION_JSON))
                    .andExpect(status().isOk())
                    .andExpect(content().contentType(MediaType.APPLICATION_JSON))
                    .andExpect(
                        MockMvcResultMatchers.jsonPath("$.status")
                            .value(DebtPositionStatus.VALID.toString())));
  }

  private PaymentPositionDTO prepareScheduledPosition(
      PaymentPositionDTO pp,
      LocalDateTime validityDate,
      LocalDateTime dueDate,
      boolean switchToExpired) {

    pp.setValidityDate(validityDate);
    pp.setSwitchToExpired(switchToExpired);

    pp.getPaymentOption().forEach(po -> {
      po.setValidityDate(validityDate);
      po.setDueDate(dueDate);
      po.setStatus(PaymentOptionStatus.PO_UNPAID);
      po.setSwitchToExpired(switchToExpired);
    });

    return pp;
  }

  private void waitUntilAfter(LocalDateTime dateTime) {
    Awaitility.await()
        .atMost(10, TimeUnit.SECONDS)
        .pollInterval(100, TimeUnit.MILLISECONDS)
        .until(() -> LocalDateTime.now(ZoneOffset.UTC).isAfter(dateTime));
  }

  private LocalDateTime nowUtc() {
    return LocalDateTime.now(ZoneOffset.UTC);
  }
}