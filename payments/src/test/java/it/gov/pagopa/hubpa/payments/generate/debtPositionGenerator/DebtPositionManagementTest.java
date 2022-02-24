package it.gov.pagopa.hubpa.payments.generate.debtPositionGenerator;

import static org.assertj.core.api.Assertions.assertThat;

import java.math.BigDecimal;
import java.util.LinkedList;
import java.util.List;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import it.gov.pagopa.hubpa.payments.generate.debtposition.DebtPositionGeneration;
import it.gov.pagopa.hubpa.payments.generate.debtposition.DebtPositionManagement;
import it.gov.pagopa.hubpa.payments.generate.debtposition.bean.DebtPosition;
import it.gov.pagopa.hubpa.payments.generate.debtposition.bean.debtposition.DPPayer;
import it.gov.pagopa.hubpa.payments.generate.debtposition.bean.debtposition.DPPaymentDetail;
import it.gov.pagopa.hubpa.payments.generate.debtposition.bean.debtposition.DPSinglePaymentDetail;
import it.gov.pagopa.hubpa.payments.generate.debtposition.enumeration.PaymentStatusEnum;
import it.gov.pagopa.hubpa.payments.generate.rpt.xsd.StTipoIdentificativoUnivocoPersFG;

/**
 * Tests on management of <code>DebtPosition</code>
 */
class DebtPositionManagementTest {

    DebtPosition debtPosition;

    /**
     * @throws java.lang.Exception
     */
    @BeforeEach
    void setUp() throws Exception {
	String uniqueIdentificationCode = "RSSMRA80A01F205X";
	StTipoIdentificativoUnivocoPersFG typeUniqueIdentifier = StTipoIdentificativoUnivocoPersFG.F;
	String registry = "ROSSI MARIO";
	String domainIdentifier = "01234567890";
	int auxDigit = 3;
	Integer segregationCode = 01;
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

	debtPosition = DebtPositionGeneration.generate(payer, paymentDetail, singlePaymentDetailList);
	debtPosition.getPaymentDetail().setNoticeNumberManual("012345678901234567890");
    }

    /**
     * Test on validity of <code>DebtPosition</code>
     * 
     * @throws Exception
     * @see DebtPosition
     * @see DebtPositionManagement
     */
    @Test
    void testValidate() throws Exception {
	DebtPositionManagement.validate(debtPosition);
    }

    /**
     * Test on status of <code>DebtPosition</code><br/>
     * The status of <code>DebtPosition</code> must become payable
     * 
     * @throws Exception
     * @see DebtPosition
     * @see DebtPositionManagement
     */
    @Test
    void testMakePayable() throws Exception {
	assertThat(DebtPositionManagement.makePayable(debtPosition).getPaymentDetail().getPaymentStatus())
		.isEqualTo(PaymentStatusEnum.PAYABLE);
    }

    /**
     * Test on status of <code>DebtPosition</code><br/>
     * The status of <code>DebtPosition</code> must become not payable
     * 
     * @throws Exception
     * @see DebtPosition
     * @see DebtPositionManagement
     */
    @Test
    void testMakeNotPayable() throws Exception {
	assertThat(DebtPositionManagement.makeNotPayable(debtPosition).getPaymentDetail().getPaymentStatus())
		.isEqualTo(PaymentStatusEnum.NOT_PAYABLE);
    }

    /**
     * Test on status of <code>DebtPosition</code><br/>
     * The status of <code>DebtPosition</code> must become canceled
     * 
     * @throws Exception
     * @see DebtPosition
     * @see DebtPositionManagement
     */
    @Test
    void testMakeCancel() throws Exception {
	assertThat(DebtPositionManagement.makeCancel(debtPosition).getPaymentDetail().getPaymentStatus())
		.isEqualTo(PaymentStatusEnum.CANCELED);
    }

    /**
     * Test on status of <code>DebtPosition</code><br/>
     * The status of <code>DebtPosition</code> must become paid
     * 
     * @throws Exception
     * @see DebtPosition
     * @see DebtPositionManagement
     */
    @Test
    void testMakePaid() throws Exception {
	assertThat(DebtPositionManagement.makePaid(debtPosition).getPaymentDetail().getPaymentStatus())
		.isEqualTo(PaymentStatusEnum.PAID);
    }
}
