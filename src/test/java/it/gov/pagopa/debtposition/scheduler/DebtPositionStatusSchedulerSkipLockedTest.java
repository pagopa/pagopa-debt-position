package it.gov.pagopa.debtposition.scheduler;

import static org.assertj.core.api.Assertions.assertThat;

import it.gov.pagopa.debtposition.DebtPositionApplication;
import it.gov.pagopa.debtposition.model.enumeration.DebtPositionStatus;
import it.gov.pagopa.debtposition.service.DebtPositionStatusBatchService;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import javax.sql.DataSource;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.test.context.DynamicPropertyRegistry;
import org.springframework.test.context.DynamicPropertySource;
import org.testcontainers.containers.PostgreSQLContainer;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;

@SpringBootTest(classes = DebtPositionApplication.class)
@Testcontainers
@Execution(ExecutionMode.SAME_THREAD)
class DebtPositionStatusSchedulerSkipLockedTest {

  @Autowired
  private DataSource dataSource;

  @Autowired
  private JdbcTemplate jdbcTemplate;

  @Autowired
  private DebtPositionStatusBatchService batchService;

  // PostgreSQL is required here because H2 does not reliably support the locking behavior
  // used by the batch query, especially FOR UPDATE SKIP LOCKED.
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
    registry.add("cron.job.schedule.enabled", () -> "false");
    registry.add("cron.job.schedule.shedlock.defaultlockatmostfor", () -> "30m");
    registry.add("cron.job.schedule.shedlock.lockatmostfor", () -> "45m");
    registry.add("cron.job.schedule.shedlock.lockatleastfor", () -> "0s");
    registry.add("cron.job.schedule.batch-size", () -> "2");
  }

  @Test
  void updatePublishedToValidBatchShouldSkipLockedRows() throws Exception {
    LocalDateTime now = LocalDateTime.now(ZoneOffset.UTC);
    LocalDateTime expiredValidityDate = now.minusMinutes(1);
    LocalDateTime futureDueDate = now.plusDays(1);

    // Insert two eligible PUBLISHED debt positions. Both match the batch query criteria.
    Long lockedPaymentPositionId =
        insertPublishedPaymentPosition(
            "ORG_SKIP_LOCKED_1",
            "IUPD_SKIP_LOCKED_1",
            "IUV_SKIP_LOCKED_1",
            "NAV_SKIP_LOCKED_1",
            expiredValidityDate,
            futureDueDate);

    Long unlockedPaymentPositionId =
        insertPublishedPaymentPosition(
            "ORG_SKIP_LOCKED_2",
            "IUPD_SKIP_LOCKED_2",
            "IUV_SKIP_LOCKED_2",
            "NAV_SKIP_LOCKED_2",
            expiredValidityDate,
            futureDueDate);

    ExecutorService executor = Executors.newSingleThreadExecutor();

    try (Connection lockingConnection = dataSource.getConnection()) {
      lockingConnection.setAutoCommit(false);

      // Keep one row locked in a separate transaction.
      // The batch query should skip this row instead of blocking on it.
      lockPaymentPosition(lockingConnection, lockedPaymentPositionId);

      // Run the batch in another thread, simulating a concurrent worker.
      Future<Integer> future =
          executor.submit(() -> batchService.updatePublishedToValidBatch(now, 2));

      Integer affectedRows = future.get(3, TimeUnit.SECONDS);

      // Only the unlocked row should be updated because the other one is skipped by SKIP LOCKED.
      assertThat(affectedRows).isEqualTo(1);

      assertThat(getStatus(lockedPaymentPositionId))
          .isEqualTo(DebtPositionStatus.PUBLISHED.toString());

      assertThat(getStatus(unlockedPaymentPositionId))
          .isEqualTo(DebtPositionStatus.VALID.toString());

      // Release the database lock without changing the locked row.
      lockingConnection.rollback();
    } finally {
      executor.shutdownNow();
    }

    // After releasing the lock, the previously skipped row should become processable.
    Integer affectedRowsAfterLockRelease =
        batchService.updatePublishedToValidBatch(now, 2);

    assertThat(affectedRowsAfterLockRelease).isEqualTo(1);

    assertThat(getStatus(lockedPaymentPositionId))
        .isEqualTo(DebtPositionStatus.VALID.toString());

    assertThat(getStatus(unlockedPaymentPositionId))
        .isEqualTo(DebtPositionStatus.VALID.toString());
  }

  // Inserts a minimal PUBLISHED payment position with one eligible unpaid payment option.
  private Long insertPublishedPaymentPosition(
      String organizationFiscalCode,
      String iupd,
      String iuv,
      String nav,
      LocalDateTime validityDate,
      LocalDateTime dueDate) {

    Long paymentPositionId =
        jdbcTemplate.queryForObject(
            """
            INSERT INTO apd.payment_position (
              id,
              iupd,
              organization_fiscal_code,
              type,
              fiscal_code,
              full_name,
              company_name,
              inserted_date,
              last_updated_date,
              min_due_date,
              max_due_date,
              publish_date,
              validity_date,
              status
            )
            VALUES (
              nextval('apd.payment_pos_seq'),
              ?,
              ?,
              'G',
              'FISCALCODE',
              'Full Name',
              'Company Name',
              ?,
              ?,
              ?,
              ?,
              ?,
              ?,
              'PUBLISHED'
            )
            RETURNING id
            """,
            Long.class,
            iupd,
            organizationFiscalCode,
            LocalDateTime.now(ZoneOffset.UTC),
            LocalDateTime.now(ZoneOffset.UTC),
            dueDate,
            dueDate,
            LocalDateTime.now(ZoneOffset.UTC),
            validityDate);

    jdbcTemplate.update(
        """
        INSERT INTO apd.payment_option (
          id,
          payment_position_id,
          iuv,
          nav,
          organization_fiscal_code,
          amount,
          description,
          is_partial_payment,
          due_date,
          retention_date,
          inserted_date,
          last_updated_date,
          fee,
          notification_fee,
          fiscal_code,
          full_name,
          status,
          type,
          validity_date
        )
        VALUES (
          nextval('apd.payment_opt_seq'),
          ?,
          ?,
          ?,
          ?,
          1000,
          'payment option description',
          false,
          ?,
          ?,
          ?,
          ?,
          0,
          0,
          'FISCALCODE',
          'Full Name',
          'PO_UNPAID',
          'G',
          ?
        )
        """,
        paymentPositionId,
        iuv,
        nav,
        organizationFiscalCode,
        dueDate,
        dueDate.plusDays(7),
        LocalDateTime.now(ZoneOffset.UTC),
        LocalDateTime.now(ZoneOffset.UTC),
        validityDate);

    return paymentPositionId;
  }

  // Opens a row-level lock and keeps it active until the caller commits or rolls back
  // the surrounding transaction.
  private void lockPaymentPosition(Connection connection, Long paymentPositionId) throws Exception {
    try (PreparedStatement statement =
        connection.prepareStatement(
            """
            SELECT id
            FROM apd.payment_position
            WHERE id = ?
            FOR UPDATE
            """)) {

      statement.setLong(1, paymentPositionId);

      try (ResultSet resultSet = statement.executeQuery()) {
        assertThat(resultSet.next()).isTrue();
      }
    }
  }

  // Reads the current status directly from the database to verify the batch side effects.
  private String getStatus(Long paymentPositionId) {
    return jdbcTemplate.queryForObject(
        """
        SELECT status
        FROM apd.payment_position
        WHERE id = ?
        """,
        String.class,
        paymentPositionId);
  }
}