package it.gov.pagopa.hubpa.payments.generate.debtposition.validation;

import java.math.BigDecimal;
import java.util.Optional;
import java.util.Set;

import javax.validation.ConstraintViolation;
import javax.validation.Validation;
import javax.validation.Validator;

import it.gov.pagopa.hubpa.payments.generate.common.Constants;
import it.gov.pagopa.hubpa.payments.iuvgenerator.common.ErrorMessages;
import it.gov.pagopa.hubpa.payments.generate.common.validation.GenericValidation;
import it.gov.pagopa.hubpa.payments.generate.debtposition.bean.DebtPosition;
import it.gov.pagopa.hubpa.payments.generate.debtposition.bean.debtposition.DPSinglePaymentDetail;
import it.gov.pagopa.hubpa.payments.generate.debtposition.exception.ValidationException;
import it.gov.pagopa.hubpa.payments.service.IuvCodeGenerator;
import it.gov.pagopa.hubpa.payments.iuvgenerator.validation.IuvCodeValidation;
import it.gov.pagopa.hubpa.payments.iuvgenerator.validation.IuvCodeValidationImpl;
import it.gov.pagopa.hubpa.payments.generate.rpt.xsd.StTipoIdentificativoUnivocoPersFG;

/**
 * Implementation of DebtPositionValidation interface
 */
public class DebtPositionValidationImpl implements DebtPositionValidation {

    /**
     * Validate the debtPosition<br/>
     * The validation includes:
     * <ul>
     * <li>checkConstraints - validation by annotation
     * <li>checkUniqueIdentificationType - if <code>uniqueIdentificationType</code>
     * = G then it must be <code>uniqueIdentificationCode</code> = valid VAT number;
     * if <code>uniqueIdentificationType</code> = F then it must be
     * <code>uniqueIdentificationCode</code> = valid fiscal code
     * <li>checkReceivedIuv - the <code>iuv</code> must be valid; check digit
     * validation
     * <li>checkSinglePaymentsDetailList - the list of
     * <code>DPSinglePaymentDetail</code> must have a maximum of 5 elements
     * <li>checkAmounts - the sum of the amounts in
     * <code>DPSinglePaymentDetail</code> must coincide with the amount in
     * <code>DPPaymentDetail</code>
     * </ul>
     * 
     * @param debtPosition the bean to validate
     * @throws ValidationException
     * @see DebtPosition
     * @see ValidationException
     */
    @Override
    public void validate(DebtPosition debtPosition) throws ValidationException {
	checkConstraints(debtPosition);

	checkUniqueIdentificationType(debtPosition);

	checkReceivedIuv(debtPosition);

	checkSinglePaymentsDetailList(debtPosition);

	checkAmounts(debtPosition);
    }

    /**
     * @param objectToValidate the bean to validate. It can be the debt position or
     *                         one of its components.
     * @throws ValidationException
     * @see ValidationException
     */
    public <T> void checkConstraints(T objectToValidate) throws ValidationException {
	Validator validator = Validation.buildDefaultValidatorFactory().getValidator();
	Set<ConstraintViolation<T>> validationInputResults = validator.validate(objectToValidate);
	if (!validationInputResults.isEmpty()) {
	    throw new ValidationException(validationInputResults);
	}
    }

    /**
     * @param debtPosition
     * @see DebtPosition
     */
    private void checkUniqueIdentificationType(DebtPosition debtPosition) {
	if (debtPosition.getPayer().getUniqueIdentificationType().equals(StTipoIdentificativoUnivocoPersFG.G)
		&& GenericValidation.checkVatNumber(debtPosition.getPayer().getUniqueIdentificationCode())) {
	    throw new ValidationException(ErrorMessages.VALIDATION_VAT_NUMBER_ERROR);

	} else if (debtPosition.getPayer().getUniqueIdentificationType().equals(StTipoIdentificativoUnivocoPersFG.F)
		&& GenericValidation.checkFiscalCode(debtPosition.getPayer().getUniqueIdentificationCode()) > 0) {
	    throw new ValidationException(ErrorMessages.VALIDATION_FISCAL_CODE_ERROR);

	}
    }

    /**
     * @param debtPosition
     * @see DebtPosition
     */
    private void checkReceivedIuv(DebtPosition debtPosition) {
	String iuv = debtPosition.getPaymentDetail().getIuv();
	int auxDigit = debtPosition.getPaymentDetail().getAuxDigit();
	Integer applicationCode = debtPosition.getPaymentDetail().getApplicationCode();
	if (iuv != null && !iuv.trim().isEmpty()) {
	    IuvCodeGenerator iuvCodeGenerator = new IuvCodeGenerator.Builder().setAuxDigit(auxDigit)
		    .setSegregationCode(debtPosition.getPaymentDetail().getSegregationCode()).build();
	    IuvCodeValidation iuvCodeValidation = new IuvCodeValidationImpl();
	    iuvCodeValidation.validate(iuvCodeGenerator);

	    BigDecimal checkDigitReceived;
	    BigDecimal checkDigitCalculated;
	    if (auxDigit == Constants.AUX_DIGIT_0) {
		if (Optional.ofNullable(applicationCode).orElse(0) == 0) {
		    throw new ValidationException(ErrorMessages.VALIDATION_APPLICATION_CODE_ERROR);
		}
		checkDigitReceived = new BigDecimal(iuv.substring(13));
		checkDigitCalculated = new BigDecimal(auxDigit + applicationCode + iuv.substring(0, 13))
			.remainder(new BigDecimal(93));
	    } else if (auxDigit == Constants.AUX_DIGIT_3) {
		checkDigitReceived = new BigDecimal(iuv.substring(15));
		checkDigitCalculated = new BigDecimal(auxDigit + iuv.substring(0, 15)).remainder(new BigDecimal(93));
	    } else {
		throw new ValidationException(ErrorMessages.VALIDATION_AUXDIGIT_ERROR);
	    }
	    if (checkDigitReceived.compareTo(checkDigitCalculated) != 0) {
		throw new ValidationException(ErrorMessages.VALIDATION_CHECK_DIGIT_ERROR);
	    }
	}
    }

    /**
     * @param debtPosition
     * @see DebtPosition
     */
    private void checkSinglePaymentsDetailList(DebtPosition debtPosition) {
	if (debtPosition.getSinglePaymentDetailList().size() > Constants.SINGLE_PAYMENT_LIST_MAX_SIZE) {
	    throw new ValidationException(ErrorMessages.VALIDATION_SINGLE_PAYMENT_LIST_SIZE_ERROR);
	}
    }

    /**
     * @param debtPosition
     * @see DebtPosition
     */
    private void checkAmounts(DebtPosition debtPosition) {
	BigDecimal amountSinglePaymentSum = debtPosition.getSinglePaymentDetailList().stream()
		.map(DPSinglePaymentDetail::getAmountSinglePayment).reduce(BigDecimal.ZERO, BigDecimal::add);
	if (debtPosition.getPaymentDetail().getTotalAmountPayment().compareTo(amountSinglePaymentSum) != 0) {
	    throw new ValidationException(ErrorMessages.VALIDATION_AMOUNTS_ERROR);
	}
    }
}
