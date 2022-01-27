package it.gov.pagopa.debtposition.validation;

import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.time.format.DateTimeFormatter;
import java.util.List;

import it.gov.pagopa.debtposition.entity.PaymentOption;
import it.gov.pagopa.debtposition.entity.PaymentPosition;
import it.gov.pagopa.debtposition.entity.Transfer;
import it.gov.pagopa.debtposition.exception.ValidationException;


public class DebtPositionValidation {

    private static final String DUE_DATE_VALIDATION_ERROR = "Dates congruence error: due_date must be >= validity_date (if provided) or due_date must be >= current_date [due_date=%s; validity_date=%s; current_date=%s]";
    private static final String RETENTION_DATE_VALIDATION_ERROR = "Dates congruence error: retention_date must be >= due_date [retention_date=%s; due_date=%s] ";
    private static final String VALIDITY_DATE_VALIDATION_ERROR = "Dates congruence error: validity_date must be >= current_date [validity_date=%s; current_date=%s] ";
    private static final String AMOUNTS_VALIDATION_ERROR = "Amounts congruence error: payment option amount must coincide with the total of the transfers amount [payment_option_amount(in cent)=%s; total_tranfers_amount(in cent)=%s]";
            

    
    private DebtPositionValidation() {
        super();
    }

    
    
    public static void checkPaymentPositionInputDataAccurancy(final List<PaymentPosition> ppList){
        for (PaymentPosition pp : ppList) {
            checkPaymentPositionDatesCongruency(pp);
        }
    }
    
    
    private static void checkPaymentPositionDatesCongruency(final PaymentPosition pp) {

        LocalDateTime today = LocalDateTime.now(ZoneOffset.UTC);
        DateTimeFormatter  dateFormatter = DateTimeFormatter.ofPattern ("yyyy-MM-dd hh:mm:ss");

        // must be validity_date ≥ current time 
        if (null != pp.getValidityDate() && pp.getValidityDate().compareTo(today) < 0) {
            throw new ValidationException(
                    String.format(VALIDITY_DATE_VALIDATION_ERROR, 
                            dateFormatter.format(pp.getValidityDate()), 
                            dateFormatter.format(today)
                            )
                    );
        }

        for (PaymentOption po : pp.getPaymentOption()) {
            // must be due_date ≥ validity_date || due_date ≥ current time 
            if (null != pp.getValidityDate() && po.getDueDate().compareTo(pp.getValidityDate()) < 0 ||
                    null == pp.getValidityDate() && po.getDueDate().compareTo(today) < 0) {
                throw new ValidationException(
                        String.format(DUE_DATE_VALIDATION_ERROR, 
                                dateFormatter.format(po.getDueDate()), 
                                (null != pp.getValidityDate() ? dateFormatter.format(pp.getValidityDate()) : null),
                                dateFormatter.format(today)
                                )
                        );
            }
            // must be retention_date ≥ due_date 
            else if (null != po.getRetentionDate() && po.getRetentionDate().compareTo(po.getDueDate()) < 0) {
                throw new ValidationException(
                        String.format(RETENTION_DATE_VALIDATION_ERROR, 
                                dateFormatter.format(po.getRetentionDate()), 
                                dateFormatter.format(po.getDueDate())
                                )
                        );
            }

            checkPaymentOptionAmounts(po);
        }


    }
    
    private static void checkPaymentOptionAmounts(final PaymentOption po) {
        long totalTranfersAmout = 0;
        long poAmount = po.getAmount();
        for (Transfer t : po.getTransfer()) {
            checkTransferCategory(t);
            checkTransferIban(t);
            totalTranfersAmout += t.getAmount();
        }

        if (poAmount != totalTranfersAmout) {
            throw new ValidationException(
                    String.format(AMOUNTS_VALIDATION_ERROR, 
                            poAmount, 
                            totalTranfersAmout
                            )
                    );
        }
        
    }
    
    

    private static void checkTransferCategory(final Transfer t) {
        String category = t.getCategory();
        //TODO Da capire come validare il dato
    }
    
    private static void checkTransferIban(final Transfer t) {
        String orgCF = t.getOrganizationFiscalCode();
        String iban  = t.getIban();
        //TODO Da capire come validare il dato
    }
    
}
