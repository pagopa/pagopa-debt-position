package it.gov.pagopa.debtposition.validation;

import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.time.format.DateTimeFormatter;
import java.util.Optional;

import it.gov.pagopa.debtposition.entity.PaymentOption;
import it.gov.pagopa.debtposition.entity.PaymentPosition;
import it.gov.pagopa.debtposition.entity.Transfer;
import it.gov.pagopa.debtposition.exception.AppError;
import it.gov.pagopa.debtposition.exception.AppException;
import it.gov.pagopa.debtposition.exception.ValidationException;
import it.gov.pagopa.debtposition.model.enumeration.DebtPositionStatus;
import it.gov.pagopa.debtposition.model.enumeration.PaymentOptionStatus;
import it.gov.pagopa.debtposition.model.enumeration.TransferStatus;
import lombok.extern.slf4j.Slf4j;

@Slf4j
public class DebtPositionValidation {

	private static final String LOG_BASE_PARAMS_DETAIL = "organizationFiscalCode= %s; iupd= %s; iuv= %s";
    private static final String DUE_DATE_VALIDATION_ERROR = "Dates congruence error: due_date must be >= validity_date (if provided) or due_date must be >= current_date [due_date=%s; validity_date=%s; current_date=%s]";
    private static final String RETENTION_DATE_VALIDATION_ERROR = "Dates congruence error: retention_date must be >= due_date [retention_date=%s; due_date=%s] ";
    private static final String VALIDITY_DATE_VALIDATION_ERROR = "Dates congruence error: validity_date must be >= current_date [validity_date=%s; current_date=%s] ";
    private static final String AMOUNTS_VALIDATION_ERROR = "Amounts congruence error: payment option amount must coincide with the total of the transfers amount [payment_option_amount(in cent)=%s; total_tranfers_amount(in cent)=%s]";
            

    
    private DebtPositionValidation() {
        super();
    }

    
    
    public static void checkPaymentPositionInputDataAccurancy(PaymentPosition pp){
            checkPaymentPositionDatesCongruency(pp);
    }
    
    public static void checkPaymentPositionPayability(PaymentPosition ppToPay, String iuv) {
    	// Verifico se la posizione debitoria è in uno stato idoneo al pagamento
    	if (DebtPositionStatus.getPaymentPosNotPayableStatus().contains(ppToPay.getStatus())){
			throw new AppException(AppError.PAYMENT_OPTION_NOT_PAYABLE, ppToPay.getOrganizationFiscalCode(), iuv);
		}
    	// Verifico se la posizione debitoria è ancora aperta
    	checkPaymentPositionOpen(ppToPay, iuv);
    	// Verifico se l'opzione di pagamento è pagabile
    	checkPaymentOptionPayable(ppToPay, iuv) ;
    }
    
    public static void checkPaymentPositionAccountability(PaymentPosition ppToReport, String iuv, String transferId) {
    	// Verifico se la posizione debitoria è in uno stato idoneo alla rendicontazione
    	if (DebtPositionStatus.getPaymentPosNotAccountableStatus().contains(ppToReport.getStatus())){
			throw new AppException(AppError.TRANSFER_NOT_ACCOUNTABLE, ppToReport.getOrganizationFiscalCode(), iuv, transferId);
		}
    	// Verifico se la transazione è rendicontabile
    	checkTransferAccountable(ppToReport, iuv, transferId);
    }

    
    
    private static void checkPaymentPositionDatesCongruency(final PaymentPosition pp) {

        LocalDateTime today = LocalDateTime.now(ZoneOffset.UTC);
        DateTimeFormatter  dateFormatter = DateTimeFormatter.ofPattern ("yyyy-MM-dd hh:mm:ss");

        // Regola 1 - must be validity_date ≥ current time 
        if (null != pp.getValidityDate() && pp.getValidityDate().compareTo(today) < 0) {
            throw new ValidationException(
                    String.format(VALIDITY_DATE_VALIDATION_ERROR, 
                            dateFormatter.format(pp.getValidityDate()), 
                            dateFormatter.format(today)
                            )
                    );
        }

        for (PaymentOption po : pp.getPaymentOption()) {
            // Regola 4 - must be due_date ≥ validity_date || due_date ≥ current time 
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
    	 //TODO Da capire come validare il dato
    	t.getCategory();
    }
    
    private static void checkTransferIban(final Transfer t) {
    	 //TODO Da capire come validare il dato
    	t.getOrganizationFiscalCode();
        t.getIban();
    }
    
    
    private static void checkPaymentPositionOpen(PaymentPosition ppToPay, String iuv) {
		for (PaymentOption po: ppToPay.getPaymentOption()) {
    		if (isPaid(po)) {
    			throw new AppException(AppError.PAYMENT_OPTION_NOT_PAYABLE, po.getOrganizationFiscalCode(), iuv);
    		}
    	}
	}
    
    private static void checkPaymentOptionPayable(PaymentPosition ppToPay, String iuv) {
    	Optional<PaymentOption> poToPay = ppToPay.getPaymentOption().stream().filter(po -> po.getIuv().equals(iuv)).findFirst();
    	if (poToPay.isEmpty()) {
    		log.error ("Obtained unexpected empty payment option - ["
    				+ String.format(LOG_BASE_PARAMS_DETAIL, 
    						ppToPay.getOrganizationFiscalCode(), 
    						ppToPay.getIupd(),
    						iuv
                            )
					+ "]");
			throw new AppException(AppError.PAYMENT_OPTION_PAY_FAILED, ppToPay.getOrganizationFiscalCode(), iuv);
		}
    	
    	if (!poToPay.get().getStatus().equals(PaymentOptionStatus.PO_UNPAID)) {
    		throw new AppException(AppError.PAYMENT_OPTION_NOT_PAYABLE, poToPay.get().getOrganizationFiscalCode(), iuv);
    	}
    	
    	// se la posizione debitoria è già in PARTIALLY_PAID e sto provando a pagare una payment option non rateizzata (isPartialPayment = false)
    	if (ppToPay.getStatus().equals(DebtPositionStatus.PARTIALLY_PAID) && Boolean.FALSE.equals(poToPay.get().getIsPartialPayment())){
    		throw new AppException(AppError.PAYMENT_OPTION_NOT_PAYABLE, poToPay.get().getOrganizationFiscalCode(), iuv);
    	}
    	
	}
    
    private static boolean isPaid(PaymentOption po) {
    	boolean paid = true;
    	if (po.getStatus().equals(PaymentOptionStatus.PO_UNPAID) ||
    		po.getIsPartialPayment().equals(true)) {
    		
    		paid = false;
    	}
    	return paid;
    }
    
    private static void checkTransferAccountable(PaymentPosition ppToReport, String iuv, String transferId) {
    	PaymentOption poToReport = ppToReport.getPaymentOption().stream().filter(po -> po.getIuv().equals(iuv)).findFirst()
    			.orElseThrow(() -> {
    				log.error ("Obtained unexpected empty payment option - ["
    				+ String.format(LOG_BASE_PARAMS_DETAIL, 
    						ppToReport.getOrganizationFiscalCode(), 
    						ppToReport.getIupd(),
    						iuv
                            )
    				+ "]");
    				throw new AppException(AppError.TRANSFER_REPORTING_FAILED, ppToReport.getOrganizationFiscalCode(), iuv, transferId);
    				});
    	
    	if (!poToReport.getStatus().equals(PaymentOptionStatus.PO_PAID) && !poToReport.getStatus().equals(PaymentOptionStatus.PO_PARTIALLY_REPORTED)) {
    		throw new AppException(AppError.TRANSFER_NOT_ACCOUNTABLE, poToReport.getOrganizationFiscalCode(), iuv, transferId);
    	}
    	
    	Transfer transferToReport = poToReport.getTransfer().stream().filter(t -> t.getIdTransfer().equals(transferId)).findFirst()
    			.orElseThrow(() -> {
    				log.error ("Obtained unexpected empty transfer - ["
    	    				+ String.format(LOG_BASE_PARAMS_DETAIL, 
    	    						ppToReport.getOrganizationFiscalCode(), 
    	    						ppToReport.getIupd(),
    	    						iuv
    	                            )
    						+ "idTransfer= "+transferId
    						+ "]");
    				throw new AppException(AppError.TRANSFER_REPORTING_FAILED, ppToReport.getOrganizationFiscalCode(), iuv, transferId);
    			});
    	
    	
    	if (!transferToReport.getStatus().equals(TransferStatus.T_UNREPORTED)) {
    		throw new AppException(AppError.TRANSFER_NOT_ACCOUNTABLE, transferToReport.getOrganizationFiscalCode(), iuv, transferId);
    	}
    	
    }
    
}
