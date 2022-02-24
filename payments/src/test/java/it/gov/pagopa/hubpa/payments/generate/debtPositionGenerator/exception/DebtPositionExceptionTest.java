package it.gov.pagopa.hubpa.payments.generate.debtPositionGenerator.exception;

import java.math.BigDecimal;
import java.util.LinkedList;
import java.util.List;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import it.gov.pagopa.hubpa.payments.generate.debtposition.DebtPositionGeneration;
import it.gov.pagopa.hubpa.payments.generate.debtposition.bean.debtposition.DPPayer;
import it.gov.pagopa.hubpa.payments.generate.debtposition.bean.debtposition.DPPaymentDetail;
import it.gov.pagopa.hubpa.payments.generate.debtposition.bean.debtposition.DPSinglePaymentDetail;
import it.gov.pagopa.hubpa.payments.generate.debtposition.exception.ValidationException;
import it.gov.pagopa.hubpa.payments.generate.rpt.xsd.StTipoIdentificativoUnivocoPersFG;

/**
 * Tests on thrown exceptions
 */
class DebtPositionExceptionTest {

    /**
     * @throws java.lang.Exception
     */
    @BeforeEach
    public void setUp() throws Exception {

    }

    /**
     * Test method on <code>ValidationException</code>
     * 
     * @throws Exception
     * @see ValidationException
     * @see DebtPositionGeneration#generate(DPPayer, DPPaymentDetail, List)
     */
    @Test
    void testValidate() throws Exception {
	Assertions.assertThrows(ValidationException.class, () -> {
	    DPPayer payer = DebtPositionGeneration.generatePayer(null, null, null, null, null, null, null, null, null,
		    null, null);
	    DPPaymentDetail paymentDetail = DebtPositionGeneration.generatePaymentDetail(null, 0, null, null, null,
		    null, null, null, null, null, null, null, null, null);
	    DPSinglePaymentDetail singlePaymentDetail = DebtPositionGeneration.generateSinglePaymentDetail(null, null,
		    null, null, null, null, null, null);
	    List<DPSinglePaymentDetail> singlePaymentDetailList = new LinkedList<DPSinglePaymentDetail>();
	    singlePaymentDetailList.add(singlePaymentDetail);

	    DebtPositionGeneration.generate(payer, paymentDetail, singlePaymentDetailList);
	});
    }

    /**
     * Test method on <code>ValidationException</code> related to received IUV code
     * 
     * @throws Exception
     * @see pagopa.gov.it.toolkit.iuvGenerator.exception.ValidationException
     * @see pagopa.gov.it.toolkit.iuvGenerator.IuvCodeGeneration
     * @see DebtPositionGeneration#generate(DPPayer, DPPaymentDetail, List)
     */
    @Test
    void testReceivedIuv() throws Exception {
	String uniqueIdentificationCode = "RSSMRA80A01F205X";
	StTipoIdentificativoUnivocoPersFG typeUniqueIdentifier = StTipoIdentificativoUnivocoPersFG.F;
	String registry = "ROSSI MARIO";
	String domainIdentifier = "01234567890";
	int auxDigit = 3;
	Integer segregationCode = null;
	String iuv = "01202000000003130";
	BigDecimal totalAmountPayment = BigDecimal.valueOf(11.23);
	String causal = "causal test";
	String debitIban = "IT58C0306914512000000046601";
	BigDecimal amountSinglePayment = BigDecimal.valueOf(11.23);
	Integer orderSinglePayment = 1;
	String causalDescriptionSinglePayment = "single payment causal";

	DPPayer payer = DebtPositionGeneration.generatePayer(uniqueIdentificationCode, typeUniqueIdentifier, registry,
		null, null, null, null, null, null, null, null);
	DPPaymentDetail paymentDetail = DebtPositionGeneration.generatePaymentDetail(domainIdentifier, auxDigit,
		segregationCode, null, iuv, null, totalAmountPayment, causal, null, null, null, null, debitIban, null);
	DPSinglePaymentDetail singlePaymentDetail = DebtPositionGeneration.generateSinglePaymentDetail(
		amountSinglePayment, orderSinglePayment, causalDescriptionSinglePayment, null, null, null, null, null);
	List<DPSinglePaymentDetail> singlePaymentDetailList = new LinkedList<DPSinglePaymentDetail>();
	singlePaymentDetailList.add(singlePaymentDetail);
	Assertions.assertThrows(it.gov.pagopa.hubpa.payments.iuvgenerator.exception.ValidationException.class, () -> {
	    DebtPositionGeneration.generate(payer, paymentDetail, singlePaymentDetailList);
	});
    }
}
