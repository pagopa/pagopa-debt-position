package it.gov.pagopa.hubpa.payments.generate.paymentnotice.business.printer.sections;

import java.io.IOException;
import java.math.BigDecimal;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Locale;

import com.itextpdf.barcodes.BarcodeDataMatrix;
import com.itextpdf.kernel.color.ColorConstants;
import com.itextpdf.kernel.pdf.PdfDocument;
import com.itextpdf.layout.border.Border;
import com.itextpdf.layout.element.Cell;
import com.itextpdf.layout.element.Image;
import com.itextpdf.layout.element.Paragraph;
import com.itextpdf.layout.element.Table;
import com.itextpdf.layout.element.Text;
import com.itextpdf.layout.property.TextAlignment;
import com.itextpdf.layout.property.VerticalAlignment;

import it.gov.pagopa.hubpa.payments.generate.debtposition.bean.DebtPosition;
import it.gov.pagopa.hubpa.payments.generate.paymentnotice.bean.PaymentNotice;
import it.gov.pagopa.hubpa.payments.generate.paymentnotice.business.PaymentNoticeBusiness;
import it.gov.pagopa.hubpa.payments.generate.paymentnotice.business.printer.PdfPaymentNoticeManagement;
import it.gov.pagopa.hubpa.payments.generate.paymentnotice.constants.PaymentNoticeConstants;

/**
 * Generates the postal section of the pdf containing all the information to
 * make a postal payment
 */
public class PostalSection {

    PaymentNotice paymentNotice = null;
    PdfDocument pdfDocument = null;
    DebtPosition referenceDebtPosition = null;
    int installmentNumber = -1;
    boolean is3InstallmentsPage = false;

    /**
     * Initialization of <code>PostalSection</code> in the absence of
     * installments
     * 
     * @param paymentNotice
     * @param pdfDocument
     * @see PaymentNotice
     */
    public PostalSection(PaymentNotice paymentNotice, PdfDocument pdfDocument) {
        super();
        this.paymentNotice = paymentNotice;
        this.pdfDocument = pdfDocument;
        this.referenceDebtPosition = PaymentNoticeBusiness
                .getReferenceDebtPosition(paymentNotice.getDebtPositionList());
    }

    /**
     * Initialization of <code>PostalSection</code> in the presence of
     * installments
     * 
     * @param paymentNotice
     * @param pdfDocument
     * @param installmentDebtPosition
     *            the debt position of the installment
     * @param installmentNumber
     *            the number of the installment
     * @param is3InstallmentsPage
     *            true if the page has 3 installments, false if the page has 2
     *            installments
     * @see PaymentNotice
     */
    public PostalSection(PaymentNotice paymentNotice, PdfDocument pdfDocument, DebtPosition installmentDebtPosition,
            int installmentNumber, boolean is3InstallmentsPage) {
        this.paymentNotice = paymentNotice;
        this.pdfDocument = pdfDocument;
        this.referenceDebtPosition = installmentDebtPosition;
        this.installmentNumber = installmentNumber;
        this.is3InstallmentsPage = is3InstallmentsPage;
    }

    /**
     * Generates the heading of <code>PostalSection</code>
     * 
     * @return
     * @throws IOException 
     */
    public Table createFirstRow() throws IOException {
        float[] colWidths = { 145, 235, 180 };
        Table table = new Table(colWidths);
        table.setHeight(26).setMargin(0).setPadding(0).setMarginLeft(10).setMarginTop(4);

        Cell cell1 = new Cell();
        cell1.setBorder(Border.NO_BORDER);
        Paragraph paragraph1 = new Paragraph();
        paragraph1.setWidthPercent(100).setMarginTop(0).setMarginLeft(10).setFixedLeading(10);
        Text text1 = new Text(PaymentNoticeConstants.PDF_TEXT_POSTAL);
        text1.setFont(PdfPaymentNoticeManagement.getTitiilliumWebBold()).setFontSize(9);
        paragraph1.add(text1);
        cell1.add(paragraph1);
        table.addCell(cell1);

        Cell cell2 = new Cell();
        cell2.setBorder(Border.NO_BORDER);
        Paragraph paragraph2 = new Paragraph();
        paragraph2.setWidthPercent(100).setMarginTop(0).setMarginLeft(10).setFixedLeading(10);
        Text text2a1 = new Text(PaymentNoticeConstants.PDF_TEXT_POSTAL_BANCO);
        text2a1.setFont(PdfPaymentNoticeManagement.getTitilliumWebRegular()).setFontSize(9);
        paragraph2.add(text2a1);
        Text text2a2 = new Text(PaymentNoticeConstants.PDF_TEXT_POSTAL_POSTA);
        text2a2.setFont(PdfPaymentNoticeManagement.getTrilliumWebBlack()).setFontSize(9);
        paragraph2.add(text2a2);
        cell2.add(paragraph2);
        table.addCell(cell2);

        Cell cell3 = new Cell();
        cell3.setBorder(Border.NO_BORDER);
        Paragraph paragraph3 = new Paragraph();
        paragraph3.setWidthPercent(100).setMarginTop(0).setMarginLeft(10).setTextAlignment(TextAlignment.RIGHT)
                .setFixedLeading(10);
        Text text3a1 = new Text(
                installmentNumber > 0 ? (installmentNumber) + PaymentNoticeConstants.PDF_TEXT_POSTAL_INSTALLMENT_NUMBER
                        : PaymentNoticeConstants.PDF_TEXT_POSTAL_SINGLE_INSTALLMENT);
        text3a1.setFont(PdfPaymentNoticeManagement.getTrilliumWebBlack()).setFontSize(9);
        paragraph3.add(text3a1);
        if (referenceDebtPosition.getPaymentDetail().getExpirationDate() != null) {
            Text text3a2 = new Text(PaymentNoticeConstants.PDF_TEXT_POSTAL_WITHIN);
            text3a2.setFont(PdfPaymentNoticeManagement.getTitiilliumWebBold()).setFontSize(9);
            paragraph3.add(text3a2);
            DateFormat formatter = new SimpleDateFormat(PaymentNoticeConstants.PDF_TEXT_EXPIRATION_DATE_FORMAT);
            Text text3a3 = new Text(
                    "    " + formatter.format(referenceDebtPosition.getPaymentDetail().getExpirationDate()));
            text3a3.setFont(PdfPaymentNoticeManagement.getTitiilliumWebBold()).setFontSize(9);
            paragraph3.add(text3a3);
        }
        cell3.add(paragraph3);
        table.addCell(cell3);

        return table;
    }

    /**
     * Generates the information part of <code>PostalSection</code>
     * 
     * @return
     * @throws IOException 
     */
    public Table createSecondRow() throws IOException {
        float[] colWidths = { 155, 310, 95 };
        Table table = new Table(colWidths);
        table.setMargin(0).setPadding(0);
        if (!is3InstallmentsPage)
            table.setHeight(193);
        else
            table.setHeight(127);

        Cell cell1 = new Cell(2, 1);
        cell1.setBorder(Border.NO_BORDER);
        Paragraph paragraph1 = new Paragraph();
        paragraph1.setWidthPercent(100).setMarginLeft(20).setFixedLeading(8);
        if (!is3InstallmentsPage)
            paragraph1.setMarginTop(105);
        else
            paragraph1.setMarginTop(60);
        StringBuilder sb = new StringBuilder(PaymentNoticeConstants.PDF_TEXT_POSTAL_INFO_PART1);
        sb.append(PaymentNoticeConstants.PDF_TEXT_POSTAL_INFO_PART2);
        sb.append(PaymentNoticeConstants.PDF_TEXT_POSTAL_INFO_PART3);
        sb.append(PaymentNoticeConstants.PDF_TEXT_POSTAL_INFO_PART4);
        Text text1a1 = new Text(sb.toString());
        text1a1.setFont(PdfPaymentNoticeManagement.getTitilliumWebRegular()).setFontSize(8);
        paragraph1.add(text1a1);
        Text text1a2 = new Text("\r" + (paymentNotice.getCreditorInstitution().getPostalAuthorizationCode() != null
                ? paymentNotice.getCreditorInstitution().getPostalAuthorizationCode() : " "));
        text1a2.setFont(PdfPaymentNoticeManagement.getTitilliumWebRegular()).setFontColor(ColorConstants.DARK_GRAY)
                .setFontSize(6);
        paragraph1.add(text1a2);
        cell1.add(paragraph1);
        table.addCell(cell1);

        Cell cell2 = new Cell();
        cell2.setMargin(0).setPadding(0).setBorder(Border.NO_BORDER);
        Paragraph paragraph2 = new Paragraph();
        paragraph2.setWidthPercent(100).setMargin(0).setPadding(0).setMarginLeft(40).setFixedLeading(11);
        if (installmentNumber < 0)
            paragraph2.setMarginTop(38);
        else if (!is3InstallmentsPage) {
            paragraph2.setMarginTop(34);
        } else
            paragraph2.setMarginTop(2);
        Text text2a1 = new Text(PaymentNoticeConstants.PDF_TEXT_POSTAL_CC);
        text2a1.setFont(PdfPaymentNoticeManagement.getTitilliumWebRegular()).setFontSize(11);
        paragraph2.add(text2a1);
        Text text2a2 = new Text(paymentNotice.getCreditorInstitution().getPostalAccountNumber());
        text2a2.setFont(PdfPaymentNoticeManagement.getRobotoFontBold()).setFontSize(11);
        paragraph2.add(text2a2);
        cell2.add(paragraph2);
        table.addCell(cell2);

        Cell cell3 = new Cell();
        cell3.setBorder(Border.NO_BORDER).setMargin(0).setPadding(0);
        Paragraph paragraph3 = new Paragraph();
        paragraph3.setWidthPercent(100).setMargin(0).setPadding(0).setFixedLeading(11)
                .setTextAlignment(TextAlignment.RIGHT).setVerticalAlignment(VerticalAlignment.BOTTOM);
        if (installmentNumber < 0)
            paragraph3.setMarginTop(38);
        else if (!is3InstallmentsPage) {
            paragraph3.setMarginTop(34);
        } else
            paragraph3.setMarginTop(2);
        Text text3a1 = new Text(PaymentNoticeConstants.PDF_TEXT_POSTAL_EURO);
        text3a1.setFont(PdfPaymentNoticeManagement.getTitilliumWebRegular()).setFontSize(11);
        paragraph3.add(text3a1);
        String amountString = String.format(Locale.ITALY, PaymentNoticeConstants.PDF_TEXT_AMOUNT_FORMAT,
                referenceDebtPosition.getPaymentDetail().getTotalAmountPayment());
        Text text3a2 = new Text(amountString);
        text3a2.setFont(PdfPaymentNoticeManagement.getRobotoFontBold()).setFontSize(11);
        paragraph3.add(text3a2);
        cell3.add(paragraph3);
        table.addCell(cell3);

        Cell cell4 = new Cell();
        cell4.setBorder(Border.NO_BORDER).setMargin(0).setPadding(0).setPaddingLeft(10);
        if (is3InstallmentsPage)
            cell4.setMarginTop(2);
        cell4.add(createPostalPaymentInfo());
        table.addCell(cell4);

        Cell cell5 = new Cell();
        cell5.setBorder(Border.NO_BORDER).setMargin(0).setPadding(0).setMarginLeft(6).setPaddingLeft(5);
        cell5.add(getPostalDataMatrix());
        table.addCell(cell5);

        return table;
    }

    /**
     * @return
     * @throws IOException 
     */
    private Table createPostalPaymentInfo() throws IOException {
        float[] colWidths = { 60, 30, 40, 40, 147 };
        Table table = new Table(colWidths);
        table.setMargin(0).setPadding(0);

        Cell cell1 = new Cell();
        cell1.setBorder(Border.NO_BORDER);
        Paragraph paragraph1 = new Paragraph();
        paragraph1.setWidthPercent(100).setMargin(0).setPadding(0).setFixedLeading(8);
        Text text1 = new Text(PaymentNoticeConstants.PDF_TEXT_POSTAL_HEADED);
        text1.setFont(PdfPaymentNoticeManagement.getTitilliumWebRegular()).setFontSize(8);
        paragraph1.add(text1);
        cell1.add(paragraph1);
        table.addCell(cell1);

        Cell cell2 = new Cell(1, 4);
        cell2.setBorder(Border.NO_BORDER);
        Paragraph paragraph2 = new Paragraph();
        paragraph2.setWidthPercent(100).setMargin(0).setPadding(0).setFixedLeading(8);
        Text text2 = new Text(paymentNotice.getCreditorInstitution().getPostalAccountHolder());
        text2.setFont(PdfPaymentNoticeManagement.getTitiilliumWebBold()).setFontSize(8);
        paragraph2.add(text2);
        cell2.add(paragraph2);
        table.addCell(cell2);

        Cell cell3 = new Cell();
        cell3.setBorder(Border.NO_BORDER);
        Paragraph paragraph3 = new Paragraph();
        paragraph3.setWidthPercent(100).setMargin(0).setPadding(0).setFixedLeading(8);
        Text text3 = new Text(PaymentNoticeConstants.PDF_TEXT_POSTAL_RECIPIENT);
        text3.setFont(PdfPaymentNoticeManagement.getTitilliumWebRegular()).setFontSize(8);
        paragraph3.add(text3);
        cell3.add(paragraph3);
        table.addCell(cell3);

        Cell cell4 = new Cell(1, 4);
        cell4.setBorder(Border.NO_BORDER);
        Paragraph paragraph4 = new Paragraph();
        paragraph4.setWidthPercent(100).setMargin(0).setPadding(0).setFixedLeading(8);
        Text text4 = new Text(paymentNotice.getDebtPositionList().get(0).getPayer().getRegistry());
        text4.setFont(PdfPaymentNoticeManagement.getTitiilliumWebBold()).setFontSize(8);
        paragraph4.add(text4);
        cell4.add(paragraph4);
        table.addCell(cell4);

        Cell cell5 = new Cell(1, 2);
        cell5.setBorder(Border.NO_BORDER);
        Paragraph paragraph5 = new Paragraph();
        paragraph5.setWidthPercent(100).setMargin(0).setPadding(0).setFixedLeading(8)
                .setVerticalAlignment(VerticalAlignment.BOTTOM);
        Text text5 = new Text(PaymentNoticeConstants.PDF_TEXT_POSTAL_PAYMENT_SUBJECT);
        text5.setFont(PdfPaymentNoticeManagement.getTitilliumWebRegular()).setFontSize(8);
        paragraph5.add(text5);
        cell5.add(paragraph5);
        table.addCell(cell5);

        Cell cell6 = new Cell(1, 3);
        cell6.setBorder(Border.NO_BORDER).setMargin(0);
        Paragraph paragraph6 = new Paragraph();
        paragraph6.setWidthPercent(100).setMargin(0).setPadding(0).setFixedLeading(8);
        Text text6 = new Text(referenceDebtPosition.getPaymentDetail().getCausal());
        text6.setFont(PdfPaymentNoticeManagement.getTitiilliumWebBold()).setFontSize(8);
        paragraph6.add(text6);
        cell6.add(paragraph6);
        table.addCell(cell6);

        Cell cell7 = new Cell(1, 3);
        cell7.setBorder(Border.NO_BORDER).setMarginTop(5);
        Paragraph paragraph7 = new Paragraph();
        paragraph7.setWidthPercent(100).setMargin(0).setPadding(0).setFixedLeading(8);
        Text text7 = new Text(PaymentNoticeConstants.PDF_TEXT_POSTAL_NOTICE_NUMBER);
        text7.setFont(PdfPaymentNoticeManagement.getTitilliumWebRegular()).setFontSize(8);
        paragraph7.add(text7);
        cell7.add(paragraph7);
        table.addCell(cell7);

        Cell cell8 = new Cell();
        cell8.setBorder(Border.NO_BORDER).setMarginTop(5);
        Paragraph paragraph8 = new Paragraph();
        paragraph8.setWidthPercent(100).setMargin(0).setPadding(0).setPaddingLeft(2).setFixedLeading(8)
                .setTextAlignment(TextAlignment.CENTER);
        Text text8 = new Text(PaymentNoticeConstants.PDF_TEXT_POSTAL_TYPE_LABEL);
        text8.setFont(PdfPaymentNoticeManagement.getTitilliumWebRegular()).setFontSize(8);
        paragraph8.add(text8);
        cell8.add(paragraph8);
        table.addCell(cell8);

        Cell cell9 = new Cell();
        cell9.setBorder(Border.NO_BORDER).setMarginTop(5);
        Paragraph paragraph9 = new Paragraph();
        paragraph9.setWidthPercent(100).setPaddingRight(5).setFixedLeading(8).setTextAlignment(TextAlignment.RIGHT);
        Text text9 = new Text(PaymentNoticeConstants.PDF_TEXT_POSTAL_CI_FISCAL_CODE);
        text9.setFont(PdfPaymentNoticeManagement.getTitilliumWebRegular()).setFontSize(8);
        paragraph9.add(text9);
        cell9.add(paragraph9);
        table.addCell(cell9);

        Cell cell10 = new Cell(1, 3);
        cell10.setBorder(Border.NO_BORDER);
        Paragraph paragraph10 = new Paragraph();
        paragraph10.setWidthPercent(100).setMargin(0).setPadding(0).setFixedLeading(8);
        Text text10 = new Text(PaymentNoticeBusiness
                .getFormattedNoticeNumber(referenceDebtPosition.getPaymentDetail().getNoticeNumber()));
        text10.setFont(PdfPaymentNoticeManagement.getTitiilliumWebBold()).setFontSize(8);
        paragraph10.add(text10);
        cell10.add(paragraph10);
        table.addCell(cell10);

        Cell cell11 = new Cell();
        cell11.setBorder(Border.NO_BORDER).setMargin(0);
        Paragraph paragraph11 = new Paragraph();
        paragraph11.setWidthPercent(100).setMargin(0).setPadding(0).setPaddingLeft(2).setFixedLeading(8)
                .setTextAlignment(TextAlignment.CENTER);
        Text text11 = new Text(PaymentNoticeConstants.PDF_TEXT_POSTAL_TYPE_VALUE);
        text11.setFont(PdfPaymentNoticeManagement.getTitiilliumWebBold()).setFontSize(8);
        paragraph11.add(text11);
        cell11.add(paragraph11);
        table.addCell(cell11);

        Cell cell12 = new Cell();
        cell12.setBorder(Border.NO_BORDER);
        Paragraph paragraph12 = new Paragraph();
        paragraph12.setWidthPercent(100).setMargin(0).setPadding(0).setPaddingRight(5).setFixedLeading(8)
                .setTextAlignment(TextAlignment.RIGHT);
        Text text12 = new Text(paymentNotice.getCreditorInstitution().getFiscalCode());
        text12.setFont(PdfPaymentNoticeManagement.getTitiilliumWebBold()).setFontSize(8);
        paragraph12.add(text12);
        cell12.add(paragraph12);
        table.addCell(cell12);

        if (!is3InstallmentsPage) {
            Cell emptyCell1 = PdfPaymentNoticeManagement.getEmptyCell(60);
            emptyCell1.setHeight(15);
            table.addCell(emptyCell1);

            Cell emptyCell2 = PdfPaymentNoticeManagement.getEmptyCell(30);
            emptyCell2.setHeight(15);
            table.addCell(emptyCell2);

            Cell emptyCell3 = PdfPaymentNoticeManagement.getEmptyCell(40);
            emptyCell3.setHeight(15);
            table.addCell(emptyCell3);

            Cell emptyCell4 = PdfPaymentNoticeManagement.getEmptyCell(40);
            emptyCell4.setHeight(15);
            table.addCell(emptyCell4);

            Cell emptyCell5 = PdfPaymentNoticeManagement.getEmptyCell(147);
            emptyCell5.setHeight(15);
            table.addCell(emptyCell5);
        }

        return table;
    }

    /**
     * @return
     */
    private Paragraph getPostalDataMatrix() {
        StringBuilder dsbDtaMatrix = new StringBuilder();
        dsbDtaMatrix.append(PaymentNoticeConstants.PDF_TEXT_POSTAL_MATRIX_INDIRIZZAMENTO_FASE);
        dsbDtaMatrix.append(PaymentNoticeConstants.PDF_TEXT_POSTAL_MATRIX_CODICE_FASE_ACCETTAZIONE);
        dsbDtaMatrix.append(PaymentNoticeConstants.PDF_TEXT_POSTAL_MATRIX_SEPARATORE);
        dsbDtaMatrix.append(createCodeline());
        dsbDtaMatrix.append(PaymentNoticeConstants.PDF_TEXT_POSTAL_MATRIX_ID);
        dsbDtaMatrix.append(PaymentNoticeConstants.PDF_TEXT_POSTAL_MATRIX_PAYMENT_PHASE);
        dsbDtaMatrix.append(paymentNotice.getCreditorInstitution().getFiscalCode());
        dsbDtaMatrix.append(String.format(PaymentNoticeConstants.PDF_TEXT_POSTAL_MATRIX_IDENTIFICATION_CODE_FORMAT,
                referenceDebtPosition.getPayer().getUniqueIdentificationCode().toUpperCase()));
        dsbDtaMatrix.append(String.format(PaymentNoticeConstants.PDF_TEXT_POSTAL_MATRIX_PAYER_REGISTRY_FORMAT,
                referenceDebtPosition.getPayer().getRegistry().toUpperCase()));
        dsbDtaMatrix.append(String.format(PaymentNoticeConstants.PDF_TEXT_POSTAL_MATRIX_PAYMENT_SUBJECT_FORMAT,
                referenceDebtPosition.getPaymentDetail().getCausal().toUpperCase()));
        dsbDtaMatrix.append(String.format(PaymentNoticeConstants.PDF_TEXT_POSTAL_MATRIX_FILLER_FORMAT,
                PaymentNoticeConstants.PDF_TEXT_POSTAL_MATRIX_FILLER_VALUE));
        dsbDtaMatrix.append(PaymentNoticeConstants.PDF_TEXT_POSTAL_MATRIX_FINAL_VALUE);

        BarcodeDataMatrix barcode = new BarcodeDataMatrix(dsbDtaMatrix.toString());
        Image img = new Image(barcode.createFormXObject(pdfDocument));
        img.setMarginLeft(10).setWidth(70).setHeight(70).setAutoScale(false);
        if (is3InstallmentsPage && installmentNumber % 3 > 0) {
            img.setMarginTop(10);
        } else if (is3InstallmentsPage) {
            img.setMarginTop(13);
        } else {
            img.setMarginTop(6);
        }
        Paragraph paragraph = new Paragraph();
        paragraph.setWidthPercent(100).setPadding(0).setMargin(0).setPaddingLeft(1);
        paragraph.add(img);

        return paragraph;
    }

    /**
     * @return
     */
    private String createCodeline() {
        StringBuilder codeline = new StringBuilder();
        codeline.append(PaymentNoticeConstants.PDF_TEXT_POSTAL_MATRIX_NOTICE_NUMBER_LENGTH);
        codeline.append(referenceDebtPosition.getPaymentDetail().getNoticeNumber());
        codeline.append(PaymentNoticeConstants.PDF_TEXT_POSTAL_MATRIX_CC_LENGTH);
        codeline.append(String.format(PaymentNoticeConstants.PDF_TEXT_POSTAL_MATRIX_ACCOUNT_NUMBER_FORMAT,
                new Long(paymentNotice.getCreditorInstitution().getPostalAccountNumber())));
        codeline.append(PaymentNoticeConstants.PDF_TEXT_POSTAL_MATRIX_AMOUNT_LENGTH);
        codeline.append(String.format(PaymentNoticeConstants.PDF_TEXT_POSTAL_MATRIX_AMOUNT_FORMAT, referenceDebtPosition
                .getPaymentDetail().getTotalAmountPayment().multiply(new BigDecimal(100)).intValue()));
        codeline.append(PaymentNoticeConstants.PDF_TEXT_POSTAL_MATRIX_DOC_TYPE_LENGTH);
        codeline.append(PaymentNoticeConstants.PDF_TEXT_POSTAL_MATRIX_DOC_VALUE);
        return codeline.toString();
    }
}
