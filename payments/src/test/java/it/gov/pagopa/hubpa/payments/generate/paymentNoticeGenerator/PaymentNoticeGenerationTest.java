package it.gov.pagopa.hubpa.payments.generate.paymentNoticeGenerator;

import static org.assertj.core.api.Assertions.assertThat;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.math.BigDecimal;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Calendar;
import java.util.Date;
import java.util.LinkedList;
import java.util.List;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import it.gov.pagopa.hubpa.payments.generate.debtposition.DebtPositionGeneration;
import it.gov.pagopa.hubpa.payments.generate.debtposition.bean.DebtPosition;
import it.gov.pagopa.hubpa.payments.generate.debtposition.bean.debtposition.DPPayer;
import it.gov.pagopa.hubpa.payments.generate.debtposition.bean.debtposition.DPPaymentDetail;
import it.gov.pagopa.hubpa.payments.generate.debtposition.bean.debtposition.DPSinglePaymentDetail;
import it.gov.pagopa.hubpa.payments.generate.paymentnotice.PaymentNoticeGeneration;
import it.gov.pagopa.hubpa.payments.generate.paymentnotice.bean.PNCreditorInstitution;
import it.gov.pagopa.hubpa.payments.generate.paymentnotice.bean.PaymentNotice;
import it.gov.pagopa.hubpa.payments.generate.rpt.xsd.StTipoIdentificativoUnivocoPersFG;



/**
 * Tests on generation of <code>PaymentNotice</code>
 */
class PaymentNoticeGenerationTest {

    PNCreditorInstitution creditorInstitution;

    String path = "src/test/resources/";
    Path logoPath = Paths.get(path + "logoTest.png");

    Boolean isModello1or2 = true;

    String ci_name = "nome Ente Creditore";
    String ci_sector = "settore Ente Creditore";
    String ci_info = "info Ente Creditore";
    String ci_fiscalCode = "01234567890";
    String ci_cbillCode = "98765";
    String ci_postalAccountHolder = "Giuseppe Verdi";
    String ci_postalAccountNumber = "001122334455";
    String ci_postalAuthorizationCode = "codice autorizzazione Postale";
    String ci_website = "www.enteCreditore.it";

    String payer_uniqueIdentificationCode = "RSSMRA80A01F205X";
    StTipoIdentificativoUnivocoPersFG payer_uniqueIdentificationType = StTipoIdentificativoUnivocoPersFG.F;
    String payer_registry = "ROSSI MARIO";
    String payer_address = "Via Roma";
    String payer_numberStreet = "1";
    String payer_locality = "Firenze";
    String payer_province = "FI";
    String payer_postalCode = "13579";

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
    void setUp() throws Exception {
        byte[] logoData = Files.readAllBytes(logoPath);
        creditorInstitution = PaymentNoticeGeneration.generateCreditorInstitution(logoData, ci_name, ci_sector, ci_info,
                ci_fiscalCode, ci_cbillCode, ci_postalAccountHolder, ci_postalAccountNumber, ci_postalAuthorizationCode,
                ci_website);
    }

    /**
     * Test method on generation of pdf of <code>paymentNotice</code>.<br/>
     * No installments.<br/>
     * All <code>paymentNotice</code> data.
     * 
     * @throws Exception
     * @see DebtPosition
     * @see PaymentNotice
     * @see PaymentNoticeGeneration#generate(List, PNCreditorInstitution)
     */
    @Test
    void testGenerate_NoInstallments_AllData() throws Exception {
        BigDecimal pd_totalAmountPayment = BigDecimal.valueOf(11.11);
        Date pd_expirationDate = addDays(new Date(), 10);
        DebtPosition debtPosition = createDebtPositionAllData(pd_totalAmountPayment, pd_expirationDate, null, null);

        List<DebtPosition> debtPositionList = new LinkedList<DebtPosition>();
        debtPositionList.add(debtPosition);

        String pdfFileName = "np_NoInstallments_AllData.pdf";
        byte[] pdfNoticePayment = generatePdfNoticePaymentFile(debtPositionList, pdfFileName, isModello1or2);

        assertThat(pdfNoticePayment).isNotNull();
    }

    /**
     * Test method on generation of pdf of <code>paymentNotice</code>.<br/>
     * No installments.<br/>
     * Only mandatory fields of <code>paymentNotice</code>.
     * 
     * @throws Exception
     * @see DebtPosition
     * @see PaymentNotice
     * @see PaymentNoticeGeneration#generate(List, PNCreditorInstitution)
     */
    @Test
    void testGenerate_NoInstallments_MinimumData() throws Exception {
        BigDecimal pd_totalAmountPayment = BigDecimal.valueOf(11.11);
        DebtPosition referenceDebtPosition = createDebtPositionMinimumData(pd_totalAmountPayment, null, null);
        
        List<DebtPosition> debtPositionList = new LinkedList<DebtPosition>();
        debtPositionList.add(referenceDebtPosition);

        String pdfFileName = "np_NoInstallments_MinimumData.pdf";
        byte[] pdfNoticePayment = generatePdfNoticePaymentFile(debtPositionList, pdfFileName, isModello1or2);

        assertThat(pdfNoticePayment).isNotNull();
    }

    /**
     * Test method on generation of pdf of <code>paymentNotice</code>.<br/>
     * 3 installments.<br/>
     * Yes Single Installment.<br/>
     * All <code>paymentNotice</code> data.
     * 
     * @throws Exception
     * @see DebtPosition
     * @see PaymentNotice
     * @see PaymentNoticeGeneration#generate(List, PNCreditorInstitution)
     */
    @Test
    void testGenerate_3Installments_WithSingleInstallment_AllData() throws Exception {
        int outputListSize = 3;
        Boolean hasSingleInstallment = true;
        List<DebtPosition> debtPositionList = createDebtPositionListAllData(hasSingleInstallment, outputListSize);

        String pdfFileName = "np_3Installments_WithSingleInstallment_AllData.pdf";
        byte[] pdfNoticePayment = generatePdfNoticePaymentFile(debtPositionList, pdfFileName, isModello1or2);

        assertThat(pdfNoticePayment).isNotNull();
    }

    /**
     * Test method on generation of pdf of <code>paymentNotice</code>.<br/>
     * 3 installments.<br/>
     * No Single Installment.<br/>
     * All <code>paymentNotice</code> data.
     * 
     * @throws Exception
     * @see DebtPosition
     * @see PaymentNotice
     * @see PaymentNoticeGeneration#generate(List, PNCreditorInstitution)
     */
    @Test
    void testGenerate_3Installments_NoSingleInstallment_AllData() throws Exception {
        int outputListSize = 3;
        Boolean hasSingleInstallment = false;
        List<DebtPosition> debtPositionList = createDebtPositionListAllData(hasSingleInstallment, outputListSize);

        String pdfFileName = "np_3Installments_NoSingleInstallment_AllData.pdf";
        byte[] pdfNoticePayment = generatePdfNoticePaymentFile(debtPositionList, pdfFileName, isModello1or2);

        assertThat(pdfNoticePayment).isNotNull();
    }

    /**
     * Test method on generation of pdf of <code>paymentNotice</code>.<br/>
     * 3 installments.<br/>
     * Yes Single Installment.<br/>
     * Only mandatory fields of <code>paymentNotice</code>.
     * 
     * @throws Exception
     * @see DebtPosition
     * @see PaymentNotice
     * @see PaymentNoticeGeneration#generate(List, PNCreditorInstitution)
     */
    @Test
    void testGenerate_3Installments_WithSingleInstallment_MinimumData() throws Exception {
        int outputListSize = 3;
        Boolean hasSingleInstallment = true;
        List<DebtPosition> debtPositionList = createDebtPositionListMinimumData(hasSingleInstallment, outputListSize);

        String pdfFileName = "np_3Installments_WithSingleInstallment_MinimumData.pdf";
        byte[] pdfNoticePayment = generatePdfNoticePaymentFile(debtPositionList, pdfFileName, isModello1or2);

        assertThat(pdfNoticePayment).isNotNull();
    }

    /**
     * Test method on generation of pdf of <code>paymentNotice</code>.<br/>
     * 3 installments.<br/>
     * No Single Installment.<br/>
     * Only mandatory fields of <code>paymentNotice</code>.
     * 
     * @throws Exception
     * @see DebtPosition
     * @see PaymentNotice
     * @see PaymentNoticeGeneration#generate(List, PNCreditorInstitution)
     */
    @Test
    void testGenerate_3Installments_NoSingleInstallment_MinimumData() throws Exception {
        int outputListSize = 3;
        Boolean hasSingleInstallment = false;
        List<DebtPosition> debtPositionList = createDebtPositionListMinimumData(hasSingleInstallment, outputListSize);

        String pdfFileName = "np_3Installments_NoSingleInstallment_MinimumData.pdf";
        byte[] pdfNoticePayment = generatePdfNoticePaymentFile(debtPositionList, pdfFileName, isModello1or2);

        assertThat(pdfNoticePayment).isNotNull();
    }

    /**
     * Test method on generation of pdf of <code>paymentNotice</code>.<br/>
     * 2 installments.<br/>
     * Yes Single Installment.<br/>
     * All <code>paymentNotice</code> data.
     * 
     * @throws Exception
     * @see DebtPosition
     * @see PaymentNotice
     * @see PaymentNoticeGeneration#generate(List, PNCreditorInstitution)
     */
    @Test
    void testGenerate_2Installments_WithSingleInstallment_AllData() throws Exception {
        int outputListSize = 2;
        Boolean hasSingleInstallment = true;
        List<DebtPosition> debtPositionList = createDebtPositionListAllData(hasSingleInstallment, outputListSize);

        String pdfFileName = "np_2Installments_WithSingleInstallment_AllData.pdf";
        byte[] pdfNoticePayment = generatePdfNoticePaymentFile(debtPositionList, pdfFileName, isModello1or2);

        assertThat(pdfNoticePayment).isNotNull();
    }

    /**
     * Test method on generation of pdf of <code>paymentNotice</code>.<br/>
     * 2 installments.<br/>
     * No Single Installment.<br/>
     * All <code>paymentNotice</code> data.
     * 
     * @throws Exception
     * @see DebtPosition
     * @see PaymentNotice
     * @see PaymentNoticeGeneration#generate(List, PNCreditorInstitution)
     */
    @Test
    void testGenerate_2Installments_NoSingleInstallment_AllData() throws Exception {
        int outputListSize = 2;
        Boolean hasSingleInstallment = false;
        List<DebtPosition> debtPositionList = createDebtPositionListAllData(hasSingleInstallment, outputListSize);

        String pdfFileName = "np_2Installments_NoSingleInstallment_AllData.pdf";
        byte[] pdfNoticePayment = generatePdfNoticePaymentFile(debtPositionList, pdfFileName, isModello1or2);

        assertThat(pdfNoticePayment).isNotNull();
    }

    /**
     * Test method on generation of pdf of <code>paymentNotice</code>.<br/>
     * 2 installments.<br/>
     * Yes Single Installment.<br/>
     * Only mandatory fields of <code>paymentNotice</code>.
     * 
     * @throws Exception
     * @see DebtPosition
     * @see PaymentNotice
     * @see PaymentNoticeGeneration#generate(List, PNCreditorInstitution)
     */
    @Test
    void testGenerate_2Installments_WithSingleInstallment_MinimumData() throws Exception {
        int outputListSize = 2;
        Boolean hasSingleInstallment = true;
        List<DebtPosition> debtPositionList = createDebtPositionListMinimumData(hasSingleInstallment, outputListSize);

        String pdfFileName = "np_2Installments_WithSingleInstallment_MinimumData.pdf";
        byte[] pdfNoticePayment = generatePdfNoticePaymentFile(debtPositionList, pdfFileName, isModello1or2);

        assertThat(pdfNoticePayment).isNotNull();
    }

    /**
     * Test method on generation of pdf of <code>paymentNotice</code>.<br/>
     * 2 installments.<br/>
     * No Single Installment.<br/>
     * Only mandatory fields of <code>paymentNotice</code>.
     * 
     * @throws Exception
     * @see DebtPosition
     * @see PaymentNotice
     * @see PaymentNoticeGeneration#generate(List, PNCreditorInstitution)
     */
    @Test
    void testGenerate_2Installments_NoSingleInstallment_MinimumData() throws Exception {
        int outputListSize = 2;
        Boolean hasSingleInstallment = false;
        List<DebtPosition> debtPositionList = createDebtPositionListMinimumData(hasSingleInstallment, outputListSize);

        String pdfFileName = "np_2Installments_NoSingleInstallments_MinimumData.pdf";
        byte[] pdfNoticePayment = generatePdfNoticePaymentFile(debtPositionList, pdfFileName, isModello1or2);

        assertThat(pdfNoticePayment).isNotNull();
    }

    /**
     * Test method on generation of pdf of <code>paymentNotice</code>.<br/>
     * No installments.<br/>
     * All <code>paymentNotice</code> data.<br/>
     * <code>isModello1or2</code> false
     * 
     * @throws Exception
     * @see DebtPosition
     * @see PaymentNotice
     * @see PaymentNoticeGeneration#generate(List, PNCreditorInstitution)
     */
    @Test
    void testGenerate_NoInstallments_AllData_NoModello1or2() throws Exception {
        Boolean isModello1or2 = false;
        BigDecimal pd_totalAmountPayment = BigDecimal.valueOf(11.11);
        Date pd_expirationDate = addDays(new Date(), 10);
        DebtPosition debtPosition = createDebtPositionAllData(pd_totalAmountPayment, pd_expirationDate, null, null);
        
        List<DebtPosition> debtPositionList = new LinkedList<DebtPosition>();
        debtPositionList.add(debtPosition);

        String pdfFileName = "np_NoInstallments_AllData.pdf_NoModello1or2.pdf";
        byte[] pdfNoticePayment = generatePdfNoticePaymentFile(debtPositionList, pdfFileName, isModello1or2);

        assertThat(pdfNoticePayment).isNotNull();
    }

    /**
     * Test method on generation of pdf of <code>paymentNotice</code>.<br/>
     * No installments.<br/>
     * Only mandatory fields of <code>paymentNotice</code>.<br/>
     * <code>isModello1or2</code> false
     * 
     * @throws Exception
     * @see DebtPosition
     * @see PaymentNotice
     * @see PaymentNoticeGeneration#generate(List, PNCreditorInstitution)
     */
    @Test
    void testGenerate_NoInstallments_MinimumData_NoModello1or2() throws Exception {
        Boolean isModello1or2 = false;
        BigDecimal pd_totalAmountPayment = BigDecimal.valueOf(11.11);
        DebtPosition referenceDebtPosition = createDebtPositionMinimumData(pd_totalAmountPayment, null, null);
        
        List<DebtPosition> debtPositionList = new LinkedList<DebtPosition>();
        debtPositionList.add(referenceDebtPosition);

        String pdfFileName = "np_NoInstallments_MinimumData_NoModello1or2.pdf";
        byte[] pdfNoticePayment = generatePdfNoticePaymentFile(debtPositionList, pdfFileName, isModello1or2);

        assertThat(pdfNoticePayment).isNotNull();
    }

    /**
     * Adds days to an input date
     * 
     * @param date
     *            date to modifiy
     * @param days
     *            days to add
     * @return
     */
    public static Date addDays(Date date, int days) {
        Calendar cal = Calendar.getInstance();
        cal.setTime(date);
        cal.add(Calendar.DATE, days);
        return cal.getTime();
    }

    /**
     * Generates a <code>DebtPosition</code> with all fields valued
     * 
     * @param pd_totalAmountPayment
     * @param pd_expirationDate
     * @param documentNumber
     * @param installmentNumber
     * @return DebtPosition
     * @throws Exception
     * @see DebtPosition
     * @see DebtPositionGeneration
     */
    private DebtPosition createDebtPositionAllData(BigDecimal pd_totalAmountPayment, Date pd_expirationDate,
            String documentNumber, Integer installmentNumber) throws Exception {
        DPPayer payer = DebtPositionGeneration.generatePayer(payer_uniqueIdentificationCode,
                payer_uniqueIdentificationType, payer_registry, payer_address, payer_numberStreet, payer_locality,
                payer_province, null, payer_postalCode, null, null);
        DPPaymentDetail paymentDetail = DebtPositionGeneration.generatePaymentDetail(pd_domainIdentifier, auxDigit,
                segregationCode, null, null, null, pd_totalAmountPayment, pd_causal, pd_expirationDate, null,
                documentNumber, installmentNumber, pd_debitIban, null);
        DPSinglePaymentDetail singlePaymentDetail = DebtPositionGeneration.generateSinglePaymentDetail(
                pd_totalAmountPayment, spd_orderSinglePayment, spd_causalDescriptionSinglePayment, null, null, null,
                null, null);
        List<DPSinglePaymentDetail> singlePaymentDetailList = new LinkedList<DPSinglePaymentDetail>();
        singlePaymentDetailList.add(singlePaymentDetail);

        DebtPosition debtPosition = DebtPositionGeneration.generate(payer, paymentDetail, singlePaymentDetailList);
        debtPosition.getPaymentDetail().setNoticeNumberManual("012345678901234567890");
        return debtPosition;
    }

    /**
     * Generates a <code>DebtPosition</code> with only mandatory fields valued
     * 
     * @param pd_totalAmountPayment
     * @param documentNumber
     * @param installmentNumber
     * @return DebtPosition
     * @throws Exception
     * @see DebtPosition
     * @see DebtPositionGeneration
     */
    private DebtPosition createDebtPositionMinimumData(BigDecimal pd_totalAmountPayment, String documentNumber,
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
        debtPosition.getPaymentDetail().setNoticeNumberManual("012345678901234567890");
        return debtPosition;
    }

    /**
     * Generates a list of <code>DebtPosition</code> with all fields valued<br/>
     * List size is defined in input
     * 
     * @param hasSingleInstallment
     *            true if the output list of <code>DebtPosition</code> must have
     *            the Single Installment
     * @param outputListSize
     *            size of output list of <code>DebtPosition</code>
     * @return list of <code>DebtPosition</code>
     * @throws Exception
     * @see DebtPosition
     */
    public List<DebtPosition> createDebtPositionListAllData(Boolean hasSingleInstallment, int outputListSize)
            throws Exception {
        List<DebtPosition> debtPositionList = new LinkedList<DebtPosition>();
        BigDecimal totalAmountPayment = BigDecimal.ZERO;
        String documentNumber = "docNumber0001";
        for (int i = 1; i <= outputListSize; i++) {
            BigDecimal pd_totalAmountPayment = BigDecimal.valueOf(11.11 * i);
            totalAmountPayment = totalAmountPayment.add(pd_totalAmountPayment);
            Date pd_expirationDate = addDays(new Date(), 10 * i);
            Integer installmentNumber = i;
            debtPositionList.add(createDebtPositionAllData(pd_totalAmountPayment, pd_expirationDate, documentNumber,
                    installmentNumber));
        }

        if (hasSingleInstallment) {
            Date si_expirationDate = addDays(new Date(), 100);
            Integer installmentNumber = 0;
            debtPositionList.add(createDebtPositionAllData(totalAmountPayment, si_expirationDate, documentNumber,
                    installmentNumber));
        }

        return debtPositionList;
    }

    /**
     * Generates a list of <code>DebtPosition</code> with only mandatory fields
     * valued<br/>
     * List size is defined in input
     * 
     * @param hasSingleInstallment
     *            true if the output list of <code>DebtPosition</code> must have
     *            the Single Installment
     * @param outputListSize
     *            size of output list of <code>DebtPosition</code>
     * @return list of <code>DebtPosition</code>
     * @throws Exception
     * @see DebtPosition
     */
    public List<DebtPosition> createDebtPositionListMinimumData(Boolean hasSingleInstallment, int outputListSize)
            throws Exception {
        List<DebtPosition> debtPositionList = new LinkedList<DebtPosition>();
        BigDecimal totalAmountPayment = BigDecimal.ZERO;
        String documentNumber = "docNumber0002";
        for (int i = 1; i <= outputListSize; i++) {
            BigDecimal pd_totalAmountPayment = BigDecimal.valueOf(11.11 * i);
            totalAmountPayment = totalAmountPayment.add(pd_totalAmountPayment);
            Integer installmentNumber = i;
            debtPositionList
                    .add(createDebtPositionMinimumData(pd_totalAmountPayment, documentNumber, installmentNumber));
        }

        if (hasSingleInstallment) {
            Integer installmentNumber = 0;
            debtPositionList.add(createDebtPositionMinimumData(totalAmountPayment, documentNumber, installmentNumber));
        }

        return debtPositionList;
    }

    /**
     * Generates the pdf of <code>paymentNotice</code>
     * 
     * @param debtPositionList
     * @param pdfFileName
     * @return
     * @throws Exception
     * @throws FileNotFoundException
     * @throws IOException
     * @see DebtPosition
     * @see PaymentNoticeGeneration#generate(List, PNCreditorInstitution)
     */
    private byte[] generatePdfNoticePaymentFile(List<DebtPosition> debtPositionList, String pdfFileName,
            Boolean isModello1or2) throws Exception, FileNotFoundException, IOException {
        byte[] printNoticeDebtPosition = PaymentNoticeGeneration.generate(debtPositionList, creditorInstitution,
                isModello1or2);
        // Activate the follow to save a local pdf file
        java.io.OutputStream out = new java.io.FileOutputStream(path + pdfFileName);
        out.write(printNoticeDebtPosition);
        out.close();
        return printNoticeDebtPosition;
    }
}
