package it.gov.pagopa.debtposition.exception;

import lombok.Getter;
import org.springframework.http.HttpStatus;


@Getter
public enum AppError {
 
	DEBT_POSITION_INPUT_DATA_ERROR (HttpStatus.BAD_REQUEST, "Error in the debt position input data", "%s"),
    DEBT_POSITION_CREATION_FAILED  (HttpStatus.INTERNAL_SERVER_ERROR, "The debt position creation is failed","Creation failed for the debt position with Organization Fiscal Code %s"),
	DEBT_POSITION_ALREADY_EXIST    (HttpStatus.CONFLICT, "The debt position alredy exist", "Already exists a debt position with Organization Fiscal Code %s"),
	DEBT_POSITION_NOT_FOUND		   (HttpStatus.NOT_FOUND, "Not found the debt position", "Not found a debt position for Organization Fiscal Code %s and IUPD %s"),
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


