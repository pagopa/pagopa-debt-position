package it.gov.pagopa.debtposition.exception;

import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.Test;
import org.springframework.http.HttpStatus;

class AppExceptionTest {

  @Test
  void constructor_statusTitleMessage_setsFieldsAndNullAppError() {
    AppException ex = new AppException(HttpStatus.BAD_REQUEST, "Bad", "detail");
    assertEquals("Bad", ex.getTitle());
    assertEquals(HttpStatus.BAD_REQUEST, ex.getHttpStatus());
    assertEquals("detail", ex.getMessage());
    assertNull(ex.getAppError());
  }

  @Test
  void constructor_statusTitleMessageWithCause_setsCauseAndNullAppError() {
    RuntimeException cause = new RuntimeException("root");
    AppException ex = new AppException(HttpStatus.INTERNAL_SERVER_ERROR, "error title", "error msg", cause);
    assertEquals("error title", ex.getTitle());
    assertEquals(HttpStatus.INTERNAL_SERVER_ERROR, ex.getHttpStatus());
    assertEquals("error msg", ex.getMessage());
    assertSame(cause, ex.getCause());
    assertNull(ex.getAppError());
  }

  @Test
  void constructor_appError_formatsMessageAndSetsAppError() {
    AppError err = AppError.PAYMENT_OPTION_NOT_FOUND; // "Not found a payment option for Organization Fiscal Code %s and NAV %s"
    AppException ex = new AppException(err, "77777777777", "ABC123");

    assertEquals(err, ex.getAppError());
    assertEquals(err.httpStatus, ex.getHttpStatus());
    assertEquals(err.title, ex.getTitle());
    assertTrue(
        ex.getMessage().contains("77777777777")
        && ex.getMessage().contains("ABC123"),
        "Message should contain formatted placeholders");
  }

  @Test
  void constructor_appErrorWithNullDetails_fallbackDoesNotThrow_andMessageIsGraceful() {
	// AppError with details = null (UNPROCESSABLE_ENTITY)
    AppError err = AppError.UNPROCESSABLE_ENTITY; // details = null
    AppException exNoArgs = new AppException(err /* no args */);
    assertEquals(err, exNoArgs.getAppError());
    // formatDetails returns "" (fallback)
    assertEquals("", exNoArgs.getMessage());

    // with 1 arg, fallback uses pattern "%s"
    AppException exOneArg = new AppException(err, "some detail");
    assertEquals("some detail", exOneArg.getMessage());

    // with cause message
    RuntimeException cause = new RuntimeException("runtime exception");
    AppException exWithCause = new AppException(err, cause, "cause");
    assertEquals("cause", exWithCause.getMessage());
    assertSame(cause, exWithCause.getCause());
  }

  @Test
  void toString_containsStatusAndTitle() {
    AppException ex = new AppException(HttpStatus.NOT_FOUND, "Not found", "nope");
    String s = ex.toString();
    assertTrue(s.contains("NOT_FOUND"));
    assertTrue(s.contains("Not found"));
  }
}