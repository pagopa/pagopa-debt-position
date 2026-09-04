package it.gov.pagopa.debtposition.exception;

import jakarta.validation.constraints.NotNull;
import java.util.Formatter;
import java.util.regex.Pattern;
import lombok.EqualsAndHashCode;
import lombok.Value;
import org.springframework.http.HttpStatus;
import org.springframework.validation.annotation.Validated;

/**
 * Custom exception.
 *
 * <p>See {@link ErrorHandler}
 */
@EqualsAndHashCode(callSuper = true)
@Value
@Validated
public class AppException extends RuntimeException {

  /** generated serialVersionUID */
  private static final long serialVersionUID = -2887745935671875027L;

  private static final Pattern PLACEHOLDER_PATTERN = Pattern.compile("%s", Pattern.LITERAL);

  /** title returned to the response when this exception occurred */
  String title;

  /** http status returned to the response when this exception occurred */
  HttpStatus httpStatus;

  /** originating AppError, present only when created via AppError-based constructors */
  AppError appError;

  /**
   * @param httpStatus HTTP status returned to the response
   * @param title title returned to the response when this exception occurred
   * @param message the detail message returend to the response
   * @param cause The cause of this {@link AppException}
   */
  public AppException(
      @NotNull HttpStatus httpStatus,
      @NotNull String title,
      @NotNull String message,
      Throwable cause) {
    super(message, cause);
    this.title = title;
    this.httpStatus = httpStatus;
    this.appError = null;
  }

  /**
   * @param httpStatus HTTP status returned to the response
   * @param title title returned to the response when this exception occurred
   * @param message the detail message returend to the response
   */
  public AppException(
      @NotNull HttpStatus httpStatus, @NotNull String title, @NotNull String message) {
    super(message);
    this.title = title;
    this.httpStatus = httpStatus;
    this.appError = null;
  }

  /**
   * @param appError Response template returned to the response
   * @param args {@link Formatter} replaces the placeholders in "details" string of {@link AppError}
   *     with the arguments. If there are more arguments than format specifiers, the extra arguments
   *     are ignored.
   */
  public AppException(@NotNull AppError appError, Object... args) {
    super(formatDetails(appError, args));
    this.httpStatus = appError.httpStatus;
    this.title = appError.title;
    this.appError = appError;
  }

  /**
   * @param appError Response template returned to the response
   * @param cause The cause of this {@link AppException}
   * @param args Arguments for the details of {@link AppError} replaced by the {@link Formatter}. If
   *     there are more arguments than format specifiers, the extra arguments are ignored.
   */
  public AppException(@NotNull AppError appError, Throwable cause, Object... args) {
    super(formatDetails(appError, args), cause);
    this.httpStatus = appError.httpStatus;
    this.title = appError.title;
    this.appError = appError;
  }

  private static String formatDetails(AppError appError, Object[] args) {
    // if appError.details is null, fallback to a single-string pattern
    String template = appError.details != null ? appError.details : "%s";
    if (args == null || args.length == 0) {
      return template.replace("%s", "");
    }
    String result = template;
    for (Object arg : args) {
      result =
          PLACEHOLDER_PATTERN.matcher(result).replaceFirst(arg != null ? arg.toString() : "null");
    }
    result = result.replace("%s", "");
    return result;
  }

  @Override
  public String toString() {
    return "AppException(" + httpStatus + ", " + title + ")" + super.toString();
  }
}
