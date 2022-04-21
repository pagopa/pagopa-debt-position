package it.gov.pagopa.payments.exception;

import lombok.Getter;
import org.springframework.http.HttpStatus;


@Getter
public enum AppError {

    
    RECEIPT_NOT_FOUND(HttpStatus.NOT_FOUND, "Not found the receipt", "Not found a receipt for Organization Fiscal Code %s and IUV %s"),
    RECEIPTS_NOT_FOUND(HttpStatus.NOT_FOUND, "Not found the receipts", "Not found receipts for Organization Fiscal Code %s and page %s"),
    RETRIEVAL_RECEIPT_FAILED(HttpStatus.INTERNAL_SERVER_ERROR, "The receipt recovery is failed", "Receipt recovery failed for the Organization Fiscal Code %s and IUV %s"),
    RETRIEVAL_RECEIPTS_FAILED(HttpStatus.INTERNAL_SERVER_ERROR, "The receipts recovery is failed", "Receipts recovery failed for the Organization Fiscal Code %s"),
    UNPROCESSABLE_RECEIPT(HttpStatus.UNPROCESSABLE_ENTITY, "The receipts recovery is failed", "The debt position is in an inappropriate state [status= %s] to provide the receipt [Organization Fiscal Code= %s, IUV= %s]"),
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


