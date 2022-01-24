package it.gov.pagopa.debtposition.exception;

import lombok.Getter;
import org.springframework.http.HttpStatus;


@Getter
public enum AppError {
 
    DEBT_POSITION_CREATION_FAILED(HttpStatus.INTERNAL_SERVER_ERROR, "The debt position creation is failed",""),
	DEBT_POSITION_ALREADY_EXIST  (HttpStatus.CONFLICT, "The debt position alredy exist", "A debt position for Organization Fiscal Code %s with %s or IUV %s already exists"),

    UNKNOWN(null, null, null);


    public final HttpStatus httpStatus;
    public final String title;
    public final String details;


    AppError(HttpStatus httpStatus, String title, String details) {
        this.httpStatus = httpStatus;
        this.title = title;
        this.details = details;
    }
}


