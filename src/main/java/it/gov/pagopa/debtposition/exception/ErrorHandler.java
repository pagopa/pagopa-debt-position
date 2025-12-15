package it.gov.pagopa.debtposition.exception;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Pattern;

import org.hibernate.exception.ConstraintViolationException;
import org.springframework.beans.TypeMismatchException;
import org.springframework.dao.DataIntegrityViolationException;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.HttpStatusCode;
import org.springframework.http.ResponseEntity;
import org.springframework.http.converter.HttpMessageNotReadableException;
import org.springframework.validation.FieldError;
import org.springframework.web.bind.MethodArgumentNotValidException;
import org.springframework.web.bind.MissingServletRequestParameterException;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.context.request.ServletWebRequest;
import org.springframework.web.context.request.WebRequest;
import org.springframework.web.method.annotation.MethodArgumentTypeMismatchException;
import org.springframework.web.servlet.mvc.method.annotation.ResponseEntityExceptionHandler;

import it.gov.pagopa.debtposition.model.OdPErrorResponse;
import it.gov.pagopa.debtposition.model.ProblemJson;
import lombok.extern.slf4j.Slf4j;

/** All Exceptions are handled by this class */
@ControllerAdvice
@Slf4j
public class ErrorHandler extends ResponseEntityExceptionHandler {

  public static final String INTERNAL_SERVER_ERROR = "INTERNAL SERVER ERROR";
  public static final String BAD_REQUEST = "BAD REQUEST";
  public static final String FOREIGN_KEY_VIOLATION = "23503";
  public static final List<HttpStatus> infoExLogLevel =
      List.of(HttpStatus.FORBIDDEN, HttpStatus.NOT_FOUND);

  /**
   * Handle if the input request is not a valid JSON
   *
   * @param ex {@link HttpMessageNotReadableException} exception raised
   * @param headers of the response
   * @param status of the response
   * @param request from frontend
   * @return a {@link ProblemJson} as response with the cause and with a 400 as HTTP status
   */
  @Override
  public ResponseEntity<Object> handleHttpMessageNotReadable(
      HttpMessageNotReadableException ex,
      HttpHeaders headers,
      HttpStatusCode status,
      WebRequest request) {
    log.warn("Input not readable: ", ex);

    // ODP branch
    if (isOdpEndpoint(request)) {
      OdPErrorResponse body = buildOdpBody(OdpProfile.SYNTAX, ex.getMessage()); // 400 / ODP-101
      return new ResponseEntity<>(body, OdpProfile.SYNTAX.http);
    }

    var errorResponse =
        ProblemJson.builder()
            .status(HttpStatus.BAD_REQUEST.value())
            .title(BAD_REQUEST)
            .detail(ex.getMessage())
            .build();
    return new ResponseEntity<>(errorResponse, HttpStatus.BAD_REQUEST);
  }

  /**
   * Handle if missing some request parameters in the request
   *
   * @param ex {@link MissingServletRequestParameterException} exception raised
   * @param headers of the response
   * @param status of the response
   * @param request from frontend
   * @return a {@link ProblemJson} as response with the cause and with a 400 as HTTP status
   */
  @Override
  public ResponseEntity<Object> handleMissingServletRequestParameter(
      MissingServletRequestParameterException ex,
      HttpHeaders headers,
      HttpStatusCode status,
      WebRequest request) {
    log.warn("Missing request parameter: ", ex);

    // ODP branch
    if (isOdpEndpoint(request)) {
      OdPErrorResponse body = buildOdpBody(OdpProfile.SYNTAX, ex.getMessage()); // 400 / ODP-101
      return new ResponseEntity<>(body, OdpProfile.SYNTAX.http);
    }

    var errorResponse =
        ProblemJson.builder()
            .status(HttpStatus.BAD_REQUEST.value())
            .title(BAD_REQUEST)
            .detail(ex.getMessage())
            .build();
    return new ResponseEntity<>(errorResponse, HttpStatus.BAD_REQUEST);
  }

  /**
   * Customize the response for TypeMismatchException.
   *
   * @param ex the exception
   * @param headers the headers to be written to the response
   * @param status the selected response status
   * @param request the current request
   * @return a {@code ResponseEntity} instance
   */
  @Override
  protected ResponseEntity<Object> handleTypeMismatch(
      TypeMismatchException ex, HttpHeaders headers, HttpStatusCode status, WebRequest request) {
    log.warn("Type mismatch: ", ex);

    String paramName =
        (ex instanceof MethodArgumentTypeMismatchException matme)
            ? matme.getName()
            : ex.getPropertyName();

    String valueStr = String.valueOf(ex.getValue()); // evita NPE se null
    String detail =
        (paramName != null)
            ? String.format("Invalid value %s for property %s", valueStr, paramName)
            : String.format("Invalid value %s (type mismatch)", valueStr);

    // ODP branch
    if (isOdpEndpoint(request)) {
      OdPErrorResponse body = buildOdpBody(OdpProfile.SYNTAX, detail); // 400 / ODP-101
      return new ResponseEntity<>(body, OdpProfile.SYNTAX.http);
    }

    var errorResponse =
        ProblemJson.builder()
            .status(HttpStatus.BAD_REQUEST.value())
            .title(BAD_REQUEST)
            .detail(detail)
            .build();

    return new ResponseEntity<>(errorResponse, HttpStatus.BAD_REQUEST);
  }

  /**
   * Handle if validation constraints are unsatisfied
   *
   * @param ex {@link MethodArgumentNotValidException} exception raised
   * @param headers of the response
   * @param status of the response
   * @param request from frontend
   * @return a {@link ProblemJson} as response with the cause and with a 400 as HTTP status
   */
  @Override
  protected ResponseEntity<Object> handleMethodArgumentNotValid(
      MethodArgumentNotValidException ex,
      HttpHeaders headers,
      HttpStatusCode status,
      WebRequest request) {
    List<String> details = new ArrayList<>();
    for (FieldError error : ex.getBindingResult().getFieldErrors()) {
      details.add(error.getField() + ": " + error.getDefaultMessage());
    }
    var detailsMessage = String.join(", ", details);
    log.warn("Input not valid: " + detailsMessage);

    // ODP branch
    if (isOdpEndpoint(request)) {
      OdPErrorResponse body = buildOdpBody(OdpProfile.SYNTAX, detailsMessage);
      return new ResponseEntity<>(body, OdpProfile.SYNTAX.http);
    }

    var errorResponse =
        ProblemJson.builder()
            .status(HttpStatus.BAD_REQUEST.value())
            .title(BAD_REQUEST)
            .detail(detailsMessage)
            .build();
    return new ResponseEntity<>(errorResponse, HttpStatus.BAD_REQUEST);
  }

  @ExceptionHandler(jakarta.validation.ConstraintViolationException.class)
  public ResponseEntity<Object> handleConstraintViolationException(
      jakarta.validation.ConstraintViolationException exception, ServletWebRequest webRequest) {
    String detailsMessage = exception.getMessage();
    log.warn("Input not valid: " + exception.getMessage());

    // ODP branch
    if (isOdpEndpoint(webRequest)) {
      OdPErrorResponse body = buildOdpBody(OdpProfile.SYNTAX, detailsMessage); // 400 / ODP-101
      return new ResponseEntity<>(body, OdpProfile.SYNTAX.http);
    }

    ProblemJson errorResponse =
        ProblemJson.builder()
            .status(HttpStatus.BAD_REQUEST.value())
            .title(BAD_REQUEST)
            .detail(detailsMessage)
            .build();
    return new ResponseEntity<>(errorResponse, HttpStatus.BAD_REQUEST);
  }

  /**
   * @param ex {@link DataIntegrityViolationException} exception raised when the SQL statement
   *     cannot be executed
   * @param request from frontend
   * @return a {@link ProblemJson} as response with the cause and with an appropriated HTTP status
   */
  @ExceptionHandler({DataIntegrityViolationException.class})
  public ResponseEntity<Object> handleDataIntegrityViolationException(
      final DataIntegrityViolationException ex, final WebRequest request) {
    ProblemJson errorResponse = null;

    if (ex.getCause() instanceof ConstraintViolationException constraintViolationException) {
      String sqlState = constraintViolationException.getSQLState();
      // check the reason of ConstraintViolationException: is true if the object is referenced by a
      // foreign key
      // more info: https://docs.oracle.com/javadb/10.8.3.0/ref/rrefexcept71493.html
      if (sqlState.equals(FOREIGN_KEY_VIOLATION)) {
        log.warn("Can't delete from Database", ex);
        errorResponse =
            ProblemJson.builder()
                .status(HttpStatus.CONFLICT.value())
                .title("Conflict with the current state of the resource")
                .detail("There is a relation with other resource. Delete it first.")
                .build();
      }
    }

    // default response
    if (errorResponse == null) {
      log.warn("Data Integrity Violation", ex);
      errorResponse =
          ProblemJson.builder()
              .status(HttpStatus.INTERNAL_SERVER_ERROR.value())
              .title(INTERNAL_SERVER_ERROR)
              .detail(ex.getMessage())
              .build();
    }

    // ODP branch
    if (isOdpEndpoint(request)) {
    	OdPErrorResponse body = buildOdpBody(OdpProfile.SYSTEM, ex.getMessage()); // 500 / ODP-103
    	return new ResponseEntity<>(body, OdpProfile.SYSTEM.http);
    }

    return new ResponseEntity<>(errorResponse, HttpStatus.valueOf(errorResponse.getStatus()));
  }

  /**
   * Handle if a {@link AppException} is raised
   *
   * @param ex {@link AppException} exception raised
   * @param request from frontend
   * @return a {@link ProblemJson} as response with the cause and with an appropriated HTTP status
   */
  @ExceptionHandler({AppException.class})
  public ResponseEntity<Object> handleAppException(
		  final AppException ex, final WebRequest request) {
	  String appExMsg =
			  (ex.getCause() != null)
			  ? String.format(
					  "App Exception raised: %s%nCause of the App Exception: %s",
					  ex.getMessage(), ex.getCause())
					  : String.format("App Exception raised: %s", ex.getMessage());

	  if (infoExLogLevel.contains(ex.getHttpStatus())) {
		  log.info(appExMsg);
	  } else {
		  log.warn(appExMsg);
	  }

	  // ODP branch
	  if (isOdpEndpoint(request)) {
		  AppError err = ex.getAppError();
		  AppError.OdpSpec spec = (err != null) ? err.odpSpec() : null;

		  if (spec != null) {
			  HttpStatus st;
			  if (spec.httpStatusOverride != null) {
				  st = spec.httpStatusOverride;
			  } else if (err.getHttpStatus() != null) {
				  st = err.getHttpStatus();
			  } else {
				  st = OdpProfile.SYSTEM.http;
			  }
			  OdPErrorResponse body = buildOdpBody(spec, st, ex.getMessage());
			  return new ResponseEntity<>(body, st);
		  }

		  OdPErrorResponse body = buildOdpBody(OdpProfile.SYSTEM, ex.getMessage());
		  return new ResponseEntity<>(body, OdpProfile.SYSTEM.http);
	  }

	  ProblemJson errorResponse = ProblemJson.builder()
			  .status(ex.getHttpStatus().value())
			  .title(ex.getTitle())
			  .detail(ex.getMessage())
			  .build();
	  return new ResponseEntity<>(errorResponse, ex.getHttpStatus());
  }

  /**
   * Handle if a {@link Exception} is raised
   *
   * @param ex {@link Exception} exception raised
   * @param request from frontend
   * @return a {@link ProblemJson} as response with the cause and with 500 as HTTP status
   */
  @ExceptionHandler({Exception.class})
  public ResponseEntity<Object> handleGenericException(
      final Exception ex, final WebRequest request) {
    log.error("Generic Exception raised:", ex);

    // ODP branch → always SYSTEM 500
    if (isOdpEndpoint(request)) {
      OdPErrorResponse body = buildOdpBody(OdpProfile.SYSTEM, ex.getMessage());
      return new ResponseEntity<>(body, OdpProfile.SYSTEM.http);
    }

    if (ex.getCause() instanceof AppException appException) {
      var errorResponse =
          ProblemJson.builder()
              .status(appException.getHttpStatus().value())
              .title(appException.getTitle())
              .detail(appException.getMessage())
              .build();
      return new ResponseEntity<>(errorResponse, appException.getHttpStatus());
    } else {
      var errorResponse =
          ProblemJson.builder()
              .status(HttpStatus.INTERNAL_SERVER_ERROR.value())
              .title(INTERNAL_SERVER_ERROR)
              .detail(ex.getMessage())
              .build();
      return new ResponseEntity<>(errorResponse, HttpStatus.INTERNAL_SERVER_ERROR);
    }
  }

  // ======================================================================
  // helper methods
  // ======================================================================

  /** Detects whether current request must return ODP-shaped errors. */
  private boolean isOdpEndpoint(WebRequest request) {
	final Pattern odpEndpointPath = Pattern.compile(".*/payment-options/organizations/[^/]+/notices/[^/]+/?$");
	if (request instanceof ServletWebRequest swr) {
		String uri = swr.getRequest().getRequestURI();
		return uri != null && odpEndpointPath.matcher(uri).matches();
	}
	return false;
  }

  /** Converts AppError.OdpSpec to an OdpProfile */
  private OdpProfile toProfile(AppError.OdpSpec spec) {
    if (spec == null) return OdpProfile.SYSTEM;

    // First try by code
    for (OdpProfile p : OdpProfile.values()) {
      if (p.code.equals(spec.appErrorCode)) {
        return p;
      }
    }
    // Then try by PAA code
    for (OdpProfile p : OdpProfile.values()) {
      if (p.paa.equals(spec.paaCode)) {
        return p;
      }
    }
    // Default fallback
    return OdpProfile.SYSTEM;
  }

  /** Builds an ODP ErrorResponse from a given profile and free text message. */
  private OdPErrorResponse buildOdpBody(OdpProfile profile, String freeText) {
    long epochSec = java.time.Instant.now().getEpochSecond();
    String dateTime =
        java.time.LocalDateTime.ofEpochSecond(epochSec, 0, java.time.ZoneOffset.UTC).toString();

    String errorMessage =
        profile.paa + ((freeText != null && !freeText.isBlank()) ? " " + freeText : "");

    return OdPErrorResponse.builder()
        .httpStatusCode(profile.http.value())
        .httpStatusDescription(profile.http.getReasonPhrase())
        .appErrorCode(profile.code)
        .timestamp(epochSec)
        .dateTime(dateTime)
        .errorMessage(errorMessage)
        .build();
  }
  
  /** Builds an ODP ErrorResponse from AppError.OdpSpec + the actual HttpStatus. */
  private OdPErrorResponse buildOdpBody(AppError.OdpSpec spec, HttpStatus http, String detailOrMsg) {
	  OdpProfile profile = toProfile(spec);

	  long epochSec = java.time.Instant.now().getEpochSecond();
	  String dateTime = java.time.LocalDateTime
			  .ofEpochSecond(epochSec, 0, java.time.ZoneOffset.UTC)
			  .toString();

	  String errorMessage = profile.paa
			  + ((detailOrMsg != null && !detailOrMsg.isBlank()) ? " " + detailOrMsg : "");
	  
	  return OdPErrorResponse.builder()
			  .httpStatusCode(http.value())
			  .httpStatusDescription(http.getReasonPhrase())
			  .appErrorCode(profile.code) 
			  .timestamp(epochSec)
			  .dateTime(dateTime)
			  .errorMessage(errorMessage)
			  .build();
  }
}
