package it.gov.pagopa.debtposition.exception;

import lombok.Getter;
import org.springframework.http.HttpStatus;


@Getter
public enum AppError {

    DEBT_POSITION_REQUEST_DATA_ERROR(HttpStatus.BAD_REQUEST, "Error in the debt position request data", "%s"),
    DEBT_POSITION_CREATION_FAILED(HttpStatus.INTERNAL_SERVER_ERROR, "The debt position creation is failed", "Creation failed for the debt position with Organization Fiscal Code %s"),
    DEBT_POSITION_UPDATE_FAILED(HttpStatus.INTERNAL_SERVER_ERROR, "The debt position update is failed", "Update failed for the debt position with Organization Fiscal Code %s"),
    DEBT_POSITION_UNIQUE_VIOLATION(HttpStatus.CONFLICT, "The debt position violated constraints of uniqueness", "Already exists a debt position for the Organization Fiscal Code %s or one of its payment options is not unique"),
    DEBT_POSITION_NOT_FOUND(HttpStatus.NOT_FOUND, "Not found the debt position", "Not found a debt position for Organization Fiscal Code %s and IUPD %s"),
    DEBT_POSITION_PAYMENT_FOUND(HttpStatus.CONFLICT, "Existing related payment found", "A payment transaction has already been carried out on the debt position with Organization Fiscal Code %s and IUPD %s"),
    DEBT_POSITION_NOT_UPDATABLE(HttpStatus.CONFLICT, "Existing related payment found or not in updatable state", "A payment transaction has already been carried out or, the debt position with Organization Fiscal Code %s and IUPD %s, is not in updatable state"),
    DEBT_POSITION_NOT_PUBLISHABLE(HttpStatus.CONFLICT, "Existing related payment found or not in publishable state", "A payment transaction has already been carried out or, the debt position with Organization Fiscal Code %s and IUPD %s, is not in publishable state"),
    DEBT_POSITION_PUBLISH_VALIDITY_DATE_MISMATCH(HttpStatus.CONFLICT, "Publish request occurred after the validity date began", "A publish request has been made after the validity date began for the debt position with Organization Fiscal Code %s and IUPD %s"),
    DEBT_POSITION_PUBLISH_DUE_DATE_MISMATCH(HttpStatus.CONFLICT, "Publish request occurred after the due date of a payment options has expired", "A publish request occurred after the due date of a payment options has expired for the debt position with Organization Fiscal Code %s and IUPD %s"),
    DEBT_POSITION_PUBLISH_FAILED(HttpStatus.INTERNAL_SERVER_ERROR, "The debt position publish is failed", "Publish failed for the debt position with Organization Fiscal Code %s and IUPD %s"),
    DEBT_POSITION_INVALIDATE_FAILED(HttpStatus.INTERNAL_SERVER_ERROR, "The debt position invalidate is failed", "Invalidate failed for the debt position with Organization Fiscal Code %s and IUPD %s"),
    DEBT_POSITION_NOT_INVALIDABLE(HttpStatus.CONFLICT, "Existing related payment found or not in invalidable state", "A payment transaction has already been carried out or, the debt position with Organization Fiscal Code %s and IUPD %s, is not in invalidable state"),
    DEBT_POSITION_NOT_RECOVERABLE(HttpStatus.BAD_REQUEST, "The debt positions cannot be recovered", "Debt positions cannot be recovered. Verify that due_date_to >= due_date_from and that the interval between the two dates not exceed the maximum number of days allowed [due_date_from= %s, due_date_to= %s, days_between_from_to= %s, max_days_interval= %s]"),
    PAYMENT_OPTION_NOT_FOUND(HttpStatus.NOT_FOUND, "Not found the payment option", "Not found a payment option for Organization Fiscal Code %s and IUV %s"),
    PAYMENT_OPTION_NOT_PAYABLE(HttpStatus.UNPROCESSABLE_ENTITY, "Not in payable state", "The payment option with Organization Fiscal Code %s and IUV %s is not in payable state"),
    PAYMENT_OPTION_ALREADY_PAID(HttpStatus.CONFLICT, "Existing related payment found", "A payment transaction has already been carried out for the Organization Fiscal Code %s and IUV %s"),
    PAYMENT_OPTION_PAY_FAILED(HttpStatus.INTERNAL_SERVER_ERROR, "The pay call for a payment option is failed", "Payment failed for the payment option with Organization Fiscal Code %s and IUV %s"),
    TRANSFER_NOT_FOUND(HttpStatus.NOT_FOUND, "Not found the transfer", "Not found a transfer for Organization Fiscal Code %s, IUV %s and TxID %s"),
    TRANSFER_REPORTING_FAILED(HttpStatus.INTERNAL_SERVER_ERROR, "The reporting for the transfer is failed", "Reporting failed for the transfer with Organization Fiscal Code %s, IUV %s and TxID %s"),
    TRANSFER_NOT_ACCOUNTABLE(HttpStatus.CONFLICT, "transfer is not in accountable state", "The transfer option with Organization Fiscal Code %s, IUV %s and TxID %s is not in payable state"),
    ORGANIZATION_NOT_FOUND(HttpStatus.NOT_FOUND, "Not found the organization", "Not found an organization for the Organization Fiscal Code %s"),
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


