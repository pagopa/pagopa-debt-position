package it.gov.pagopa.payments.exception;

import java.util.Formatter;

import javax.validation.constraints.NotNull;

import org.springframework.http.HttpStatus;
import org.springframework.validation.annotation.Validated;

import lombok.Getter;

/**
 * Custom exception.
 * <p> See {@link ErrorHandler}
 */
@Validated
@Getter
public class AppException extends RuntimeException {

    /**
     * generated serialVersionUID
     */
    private static final long serialVersionUID = -2887745935671875027L;

    /**
     * title returned to the response when this exception occurred
     */
    private final String title;

    /**
     * http status returned to the response when this exception occurred
     */
    private final HttpStatus httpStatus;

    /**
     * @param appError Response template returned to the response
     * @param args     {@link Formatter} replaces the placeholders in "details" string of {@link AppError} with the arguments.
     *                 If there are more arguments than format specifiers, the extra arguments are ignored.
     */
    public AppException(@NotNull AppError appError, Object... args) {
        super(formatDetails(appError, args));
        this.httpStatus = appError.httpStatus;
        this.title = appError.title;
    }


    private static String formatDetails(AppError appError, Object[] args) {
        return String.format(appError.details, args);
    }

}
