package it.gov.pagopa.hubpa.payments.generate.paymentNoticeGenerator.exception;

import java.math.BigDecimal;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.LinkedList;
import java.util.List;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import it.gov.pagopa.hubpa.payments.generate.debtposition.DebtPositionGeneration;
import it.gov.pagopa.hubpa.payments.generate.debtposition.bean.DebtPosition;
import it.gov.pagopa.hubpa.payments.generate.debtposition.bean.debtposition.DPPayer;
import it.gov.pagopa.hubpa.payments.generate.debtposition.bean.debtposition.DPPaymentDetail;
import it.gov.pagopa.hubpa.payments.generate.debtposition.bean.debtposition.DPSinglePaymentDetail;
import it.gov.pagopa.hubpa.payments.generate.paymentnotice.PaymentNoticeGeneration;
import it.gov.pagopa.hubpa.payments.generate.paymentnotice.bean.PNCreditorInstitution;
import it.gov.pagopa.hubpa.payments.generate.paymentnotice.bean.PNCreditorInstitution.Builder;
import it.gov.pagopa.hubpa.payments.generate.paymentnotice.exception.ValidationException;
import it.gov.pagopa.hubpa.payments.generate.rpt.xsd.StTipoIdentificativoUnivocoPersFG;

/**
 * Tests on thrown exceptions
 */
class ValidationExceptionTest {

    Builder creditorInstitutionBuilder = new PNCreditorInstitution.Builder();
    String path = "src/test/resources/";
    Path logoPath = Paths.get(path + "logoTest.png");
    Boolean isModello1or2 = true;

    String ci_name = "nome Ente Creditore";
    String ci_sector = "settore Ente Creditore";
    String ci_info = "info Ente Creditore";
    String ci_fiscalCode = "01234567890";
    String ci_cbillCode = "98765";
    String ci_website = "www.enteCreditore.it";

    String payer_uniqueIdentificationCode = "RSSMRA80A01F205X";
    StTipoIdentificativoUnivocoPersFG payer_uniqueIdentificationType = StTipoIdentificativoUnivocoPersFG.F;
    String payer_registry = "ROSSI MARIO";

    int auxDigit = 3;
    Integer segregationCode = 01;
    String pd_domainIdentifier = "01234567890";
    String pd_causal = "Causale pagamento";
    String pd_debitIban = "IT58C0306914512000000046601";

    Integer spd_orderSinglePayment = 1;
    String spd_causalDescriptionSinglePayment = "Causale pagamento SinglePayment";

    /**
     * @throws Exception
     */
    @BeforeEach
    public void setUp() throws Exception {
	byte[] logoData = Files.readAllBytes(logoPath);
	creditorInstitutionBuilder.setCbillCode(ci_cbillCode).setFiscalCode(ci_fiscalCode).setInfo(ci_info)
		.setName(ci_name).setSector(ci_sector).setLogo(logoData).setWebsite(ci_website);
    }

    /**
     * Test method on invalid <code>totalAmountPayment</code> when the installments
     * are absent
     * 
     * @throws Exception
     * @see ValidationException
     * @see DebtPosition
     * @see PaymentNoticeGeneration#generate(List, PNCreditorInstitution)
     */
    @Test
    void testValidate() throws Exception {
	BigDecimal pd_totalAmountPayment = BigDecimal.valueOf(11.11);
	DebtPosition debtPosition = createDebtPosition(pd_totalAmountPayment, null, null);
	List<DebtPosition> debtPositionList = new LinkedList<DebtPosition>();
	debtPositionList.add(debtPosition);
	Assertions.assertThrows(ValidationException.class, () -> {
	    PaymentNoticeGeneration.generate(debtPositionList, null, isModello1or2);
	});
    }

    /**
     * Test method on invalid <code>documentNumber</code>
     * 
     * @throws Exception
     * @see ValidationException
     * @see DebtPosition
     * @see PaymentNoticeGeneration#generate(List, PNCreditorInstitution)
     */
    @Test
    void testInvalidDocumentNumber() throws Exception {
	BigDecimal pd_totalAmountPayment = BigDecimal.valueOf(11.11);
	String documentNumber = "docNumber0001";
	DebtPosition debtPosition = createDebtPosition(pd_totalAmountPayment, documentNumber, null);
	List<DebtPosition> debtPositionList = new LinkedList<DebtPosition>();
	debtPositionList.add(debtPosition);
	Assertions.assertThrows(ValidationException.class, () -> {
	    PaymentNoticeGeneration.generate(debtPositionList, creditorInstitutionBuilder.build(), isModello1or2);
	});
    }

    /**
     * Test method on invalid <code>installmentNumber</code>
     * 
     * @throws Exception
     * @see ValidationException
     * @see DebtPosition
     * @see PaymentNoticeGeneration#generate(List, PNCreditorInstitution)
     */
    @Test
    void testInvalidInstallmentNumber() throws Exception {
	BigDecimal pd_totalAmountPayment = BigDecimal.valueOf(11.11);
	Integer installmentNumber = 1;
	DebtPosition debtPosition = createDebtPosition(pd_totalAmountPayment, null, installmentNumber);
	List<DebtPosition> debtPositionList = new LinkedList<DebtPosition>();
	debtPositionList.add(debtPosition);
	Assertions.assertThrows(ValidationException.class, () -> {
	    PaymentNoticeGeneration.generate(debtPositionList, creditorInstitutionBuilder.build(), isModello1or2);
	});
    }

    /**
     * Test method on missing installment
     * 
     * @throws Exception
     * @see ValidationException
     * @see DebtPosition
     * @see PaymentNoticeGeneration#generate(List, PNCreditorInstitution)
     */
    @Test
    void testMissingInstallmentNumber() throws Exception {
	int outputListSize = 3;// it must be grater then 2
	Boolean hasSingleInstallment = false;
	List<DebtPosition> debtPositionList = createDebtPositionListWithoutInstallmentNumberOne(hasSingleInstallment,
		outputListSize);
	Assertions.assertThrows(ValidationException.class, () -> {
	    PaymentNoticeGeneration.generate(debtPositionList, creditorInstitutionBuilder.build(), isModello1or2);
	});
    }

    /**
     * Generates a <code>DebtPosition</code> with only mandatory fields
     * 
     * @param pd_totalAmountPayment
     * @param documentNumber
     * @param installmentNumber
     * @return DebtPosition
     * @throws Exception
     * @see DebtPosition
     * @see DebtPositionGeneration
     */
    private DebtPosition createDebtPosition(BigDecimal pd_totalAmountPayment, String documentNumber,
	    Integer installmentNumber) throws Exception {
	DPPayer payer = DebtPositionGeneration.generatePayer(payer_uniqueIdentificationCode,
		payer_uniqueIdentificationType, payer_registry, null, null, null, null, null, null, null, null);
	DPPaymentDetail paymentDetail = DebtPositionGeneration.generatePaymentDetail(pd_domainIdentifier, auxDigit,
		segregationCode, null, null, null, pd_totalAmountPayment, pd_causal, null, null, documentNumber,
		installmentNumber, pd_debitIban, null);
	DPSinglePaymentDetail singlePaymentDetail = DebtPositionGeneration.generateSinglePaymentDetail(
		pd_totalAmountPayment, spd_orderSinglePayment, spd_causalDescriptionSinglePayment, null, null, null,
		null, null);
	List<DPSinglePaymentDetail> singlePaymentDetailList = new LinkedList<DPSinglePaymentDetail>();
	singlePaymentDetailList.add(singlePaymentDetail);

	DebtPosition debtPosition = DebtPositionGeneration.generate(payer, paymentDetail, singlePaymentDetailList);

	return debtPosition;
    }

    /**
     * Generates an invalid list of <code>DebtPosition</code> without installment
     * number one
     * 
     * @param hasSingleInstallment
     * @param outputListSize
     * @return
     * @throws Exception
     */
    public List<DebtPosition> createDebtPositionListWithoutInstallmentNumberOne(Boolean hasSingleInstallment,
	    int outputListSize) throws Exception {
	List<DebtPosition> debtPositionList = new LinkedList<DebtPosition>();
	BigDecimal totalAmountPayment = BigDecimal.ZERO;
	String documentNumber = "docNumber0001";
	for (int i = 2; i <= outputListSize; i++) {
	    BigDecimal pd_totalAmountPayment = BigDecimal.valueOf(11.11 * i);
	    totalAmountPayment = totalAmountPayment.add(pd_totalAmountPayment);
	    Integer installmentNumber = i;
	    debtPositionList.add(createDebtPosition(pd_totalAmountPayment, documentNumber, installmentNumber));
	}

	if (hasSingleInstallment) {
	    Integer installmentNumber = 0;
	    debtPositionList.add(createDebtPosition(totalAmountPayment, documentNumber, installmentNumber));
	}

	return debtPositionList;
    }
}
