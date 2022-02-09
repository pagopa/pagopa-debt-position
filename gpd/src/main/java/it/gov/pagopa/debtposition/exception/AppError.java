package it.gov.pagopa.debtposition.exception;

import org.springframework.http.HttpStatus;

import lombok.Getter;


@Getter
public enum AppError {
 
    DEBT_POSITION_REQUEST_DATA_ERROR    (HttpStatus.BAD_REQUEST, "Error in the debt position request data", "%s"),
    DEBT_POSITION_CREATION_FAILED       (HttpStatus.INTERNAL_SERVER_ERROR, "The debt position creation is failed","Creation failed for the debt position with Organization Fiscal Code %s"),
    DEBT_POSITION_UPDATE_FAILED         (HttpStatus.INTERNAL_SERVER_ERROR, "The debt position update is failed","Update failed for the debt position with Organization Fiscal Code %s"),
    DEBT_POSITION_ALREADY_EXIST         (HttpStatus.CONFLICT, "The debt position alredy exist", "Already exists a debt position with Organization Fiscal Code %s"),
    DEBT_POSITION_NOT_FOUND             (HttpStatus.NOT_FOUND, "Not found the debt position", "Not found a debt position for Organization Fiscal Code %s and IUPD %s"),
    DEBT_POSITION_PAYMENT_FOUND         (HttpStatus.CONFLICT, "Existing related payment found", "A payment transaction has already been carried out on the debt position with Organization Fiscal Code %s and IUPD %s"),
    DEBT_POSITION_NOT_UPDATABLE         (HttpStatus.CONFLICT, "Existing related payment found or not in updatable state", "A payment transaction has already been carried out or, the debt position with Organization Fiscal Code %s and IUPD %s, is not in updatable state"),
    DEBT_POSITION_NOT_PUBLISHABLE       (HttpStatus.CONFLICT, "Existing related payment found or not in publishable state", "A payment transaction has already been carried out or, the debt position with Organization Fiscal Code %s and IUPD %s, is not in publishable state"),
    DEBT_POSITION_PUBLISH_DATE_MISMATCH (HttpStatus.CONFLICT, "Publish request occurred after the validity date has expired", "A publish request has been made after the validity date has expired for the debt position with Organization Fiscal Code %s and IUPD %s"),
    DEBT_POSITION_PUBLISH_FAILED        (HttpStatus.INTERNAL_SERVER_ERROR, "The debt position publish is failed","Publish failed for the debt position with Organization Fiscal Code %s and IUPD %s"),
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


