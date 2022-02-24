package it.gov.pagopa.hubpa.payments.generate.paymentnotice.business.printer.sections;

import java.io.IOException;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Locale;

import com.itextpdf.barcodes.BarcodeQRCode;
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
import it.gov.pagopa.hubpa.payments.generate.paymentnotice.business.printer.QrCodeBusiness;
import it.gov.pagopa.hubpa.payments.generate.paymentnotice.constants.PaymentNoticeConstants;

/**
 * Generates the banking section of the pdf containing all the information to
 * make a bank payment
 */
public class BankingSection {

    PaymentNotice paymentNotice = null;
    PdfDocument pdfDocument = null;
    DebtPosition referenceDebtPosition = null;

    /**
     * Initialization of <code>BankingSection</code>
     * 
     * @param paymentNotice
     * @param pdfDocument
     * @see PaymentNotice
     */
    public BankingSection(PaymentNotice paymentNotice, PdfDocument pdfDocument) {
        super();
        this.paymentNotice = paymentNotice;
        this.pdfDocument = pdfDocument;
        this.referenceDebtPosition = PaymentNoticeBusiness
                .getReferenceDebtPosition(paymentNotice.getDebtPositionList());
    }

    /**
     * Generates the heading of <code>BankingSection</code>
     * 
     * @return
     * @throws IOException 
     */
    public Table createFirstRow() throws IOException {
        float[] colWidths = { 145, 235, 180 };
        Table table = new Table(colWidths);
        table.setHeight(25).setMargin(0).setMarginTop(2).setPadding(0).setMarginLeft(10);

        Cell cell1 = new Cell();
        cell1.setBorder(Border.NO_BORDER);
        Paragraph paragraph1 = new Paragraph();
        paragraph1.setWidthPercent(100).setPaddingTop(3).setMarginLeft(10).setFixedLeading(10);
        Text text1 = new Text(PaymentNoticeConstants.PDF_TEXT_BANK_OTHER_CHANNEL);
        text1.setFont(PdfPaymentNoticeManagement.getTitiilliumWebBold()).setFontSize(9);
        paragraph1.add(text1);
        cell1.add(paragraph1);
        table.addCell(cell1);

        table.addCell(PdfPaymentNoticeManagement.getEmptyCell(235));

        Cell cell2 = new Cell();
        cell2.setBorder(Border.NO_BORDER);
        Paragraph paragraph2 = new Paragraph();
        paragraph2.setWidthPercent(100).setPaddingTop(3).setMarginLeft(10).setTextAlignment(TextAlignment.RIGHT)
                .setFixedLeading(10);
        Text text2a1 = new Text(PaymentNoticeConstants.PDF_TEXT_BANK_SINGLE_INSTALLMENT);
        text2a1.setFont(PdfPaymentNoticeManagement.getTrilliumWebBlack()).setFontSize(9);
        paragraph2.add(text2a1);
        Date expirationDate = PaymentNoticeBusiness.getExpirationDate(paymentNotice.getDebtPositionList());
        if (expirationDate != null) {
            Text text2a2 = new Text(PaymentNoticeConstants.PDF_TEXT_BANK_WITHIN);
            text2a2.setFont(PdfPaymentNoticeManagement.getTitiilliumWebBold()).setFontSize(9);
            paragraph2.add(text2a2);
            DateFormat formatter = new SimpleDateFormat(PaymentNoticeConstants.PDF_TEXT_EXPIRATION_DATE_FORMAT);
            Text text2a3 = new Text("    " + formatter.format(expirationDate));
            text2a3.setFont(PdfPaymentNoticeManagement.getTitiilliumWebBold()).setFontSize(9);
            paragraph2.add(text2a3);
        }
        cell2.add(paragraph2);
        table.addCell(cell2);

        return table;
    }

    /**
     * Generates the information part of <code>BankingSection</code>
     * 
     * @return
     * @throws IOException 
     */
    public Table createSecondRow() throws IOException {
        float[] colWidths = { 95, 70, 340 };
        Table table = new Table(colWidths);
        table.setHeight(117).setMarginLeft(10).setMarginTop(0).setBorder(Border.NO_BORDER);

        Cell cell1 = PdfPaymentNoticeManagement.getEmptyCell(135);
        cell1.setBorder(Border.NO_BORDER);
        Paragraph paragraph1 = new Paragraph();
        paragraph1.setWidthPercent(100).setMarginTop(15).setMarginLeft(10).setFixedLeading(10);
        Text text1a1 = new Text(PaymentNoticeConstants.PDF_TEXT_BANK_PAYMENT_CODES_PART1);
        text1a1.setFont(PdfPaymentNoticeManagement.getTitilliumWebRegular()).setFontSize(9);
        paragraph1.add(text1a1);
        Text text1a2 = new Text(PaymentNoticeConstants.PDF_TEXT_BANK_PAYMENT_CODES_PART2);
        text1a2.setFont(PdfPaymentNoticeManagement.getTitiilliumWebBold()).setFontSize(9);
        paragraph1.add(text1a2);
        Text text1a3 = new Text(PaymentNoticeConstants.PDF_TEXT_BANK_PAYMENT_CODES_PART3);
        text1a3.setFont(PdfPaymentNoticeManagement.getTitilliumWebRegular()).setFontSize(9);
        paragraph1.add(text1a3);
        Text text1a4 = new Text(PaymentNoticeConstants.PDF_TEXT_BANK_PAYMENT_CODES_PART4);
        text1a4.setFont(PdfPaymentNoticeManagement.getTitiilliumWebBold()).setFontSize(9);
        paragraph1.add(text1a4);
        Text text1a5 = new Text(PaymentNoticeConstants.PDF_TEXT_BANK_PAYMENT_CODES_PART5);
        text1a5.setFont(PdfPaymentNoticeManagement.getTitilliumWebRegular()).setFontSize(9);
        paragraph1.add(text1a5);
        cell1.add(paragraph1);
        table.addCell(cell1);

        Cell cell2 = new Cell();
        cell2.setBorder(Border.NO_BORDER).setMargin(0).setPadding(0);
        BarcodeQRCode barcodeQRCode = new BarcodeQRCode(QrCodeBusiness.createQrCode(referenceDebtPosition,
                paymentNotice.getCreditorInstitution().getFiscalCode()));
        Image img = new Image(barcodeQRCode.createFormXObject(pdfDocument));
        img.setMarginTop(10).setMarginLeft(2).setWidth(75).setHeight(75).setAutoScale(false);
        Paragraph paragraph2 = new Paragraph();
        paragraph2.setWidthPercent(100).add(img);
        cell2.add(paragraph2);
        table.addCell(cell2);

        Cell cell3 = new Cell();
        cell3.setBorder(Border.NO_BORDER).setMargin(0).setPadding(0);
        Table innerTable = createBankingPaymentInfo();
        Paragraph paragraph3 = new Paragraph();
        paragraph3.setWidthPercent(100).setPaddingTop(15).setPaddingLeft(10);
        paragraph3.add(innerTable);
        cell3.add(paragraph3);
        table.addCell(cell3);

        return table;
    }

    /**
     * @return
     * @throws IOException 
     */
    private Table createBankingPaymentInfo() throws IOException {
        float[] colWidths = { 60, 35, 120, 100 };
        Table table = new Table(colWidths);
        table.setMargin(0).setPadding(0);

        Cell cell1 = new Cell();
        cell1.setBorder(Border.NO_BORDER).setMargin(0).setPadding(0);
        Paragraph paragraph1 = new Paragraph();
        paragraph1.setWidthPercent(100).setMargin(0).setMarginLeft(1).setPadding(0).setFixedLeading(10);
        Text text1 = new Text(PaymentNoticeConstants.PDF_TEXT_BANK_RECIPIENT);
        text1.setFont(PdfPaymentNoticeManagement.getTitilliumWebRegular()).setFontSize(8);
        paragraph1.add(text1);
        cell1.add(paragraph1);
        table.addCell(cell1);

        Cell cell2 = new Cell(1, 2);
        cell2.setBorder(Border.NO_BORDER).setMargin(0).setPadding(0);
        Paragraph paragraph2 = new Paragraph();
        paragraph2.setWidthPercent(100).setMargin(0).setMarginLeft(1).setPadding(0).setFixedLeading(10);
        Text text2 = new Text(referenceDebtPosition.getPayer().getRegistry());
        text2.setFont(PdfPaymentNoticeManagement.getTitiilliumWebBold()).setFontSize(8);
        paragraph2.add(text2);
        cell2.add(paragraph2);
        table.addCell(cell2);

        Cell cell3 = new Cell(2, 1);
        cell3.setBorder(Border.NO_BORDER).setMargin(0).setPadding(0);
        Paragraph paragraph3 = new Paragraph();
        paragraph3.setWidthPercent(100).setMargin(0).setPadding(0).setFixedLeading(11)
                .setTextAlignment(TextAlignment.RIGHT).setVerticalAlignment(VerticalAlignment.TOP);
        Text text3a1 = new Text(PaymentNoticeConstants.PDF_TEXT_BANK_EURO);
        text3a1.setFont(PdfPaymentNoticeManagement.getTitilliumWebRegular()).setFontSize(11);
        paragraph3.add(text3a1);
        String amountString = String.format(Locale.ITALY, PaymentNoticeConstants.PDF_TEXT_AMOUNT_FORMAT,
                PaymentNoticeBusiness.getPaymentTotaleAmount(paymentNotice.getDebtPositionList()));
        Text text3a2 = new Text(amountString);
        text3a2.setFont(PdfPaymentNoticeManagement.getRobotoFontBold()).setFontSize(11);
        paragraph3.add(text3a2);
        cell3.add(paragraph3);
        table.addCell(cell3);

        Cell cell4 = new Cell();
        cell4.setBorder(Border.NO_BORDER).setMargin(0).setPadding(0);
        Paragraph paragraph4 = new Paragraph();
        paragraph4.setWidthPercent(100).setMargin(0).setMarginLeft(1).setPadding(0).setFixedLeading(10);
        Text text4 = new Text(PaymentNoticeConstants.PDF_TEXT_BANK_CREDITOR_INSTITUTION);
        text4.setFont(PdfPaymentNoticeManagement.getTitilliumWebRegular()).setFontSize(8);
        paragraph4.add(text4);
        cell4.add(paragraph4);
        table.addCell(cell4);

        Cell cell5 = new Cell(1, 2);
        cell5.setBorder(Border.NO_BORDER).setMargin(0).setPadding(0);
        Paragraph paragraph5 = new Paragraph();
        paragraph5.setWidthPercent(100).setMargin(0).setMarginLeft(1).setPadding(0).setFixedLeading(10);
        Text text5 = new Text(paymentNotice.getCreditorInstitution().getName());
        text5.setFont(PdfPaymentNoticeManagement.getTitiilliumWebBold()).setFontSize(8);
        paragraph5.add(text5);
        cell5.add(paragraph5);
        table.addCell(cell5);

        Cell cell6 = new Cell(1, 2);
        cell6.setBorder(Border.NO_BORDER).setMargin(0).setPadding(0);
        Paragraph paragraph6 = new Paragraph();
        paragraph6.setWidthPercent(100).setMargin(0).setMarginLeft(1).setPadding(0).setFixedLeading(10);
        Text text6 = new Text(PaymentNoticeConstants.PDF_TEXT_BANK_PAYMENT_SUBJECT);
        text6.setFont(PdfPaymentNoticeManagement.getTitilliumWebRegular()).setFontSize(8);
        paragraph6.add(text6);
        cell6.add(paragraph6);
        table.addCell(cell6);

        Cell cell7 = new Cell(1, 2);
        cell7.setBorder(Border.NO_BORDER).setMargin(0).setPadding(0);
        Paragraph paragraph7 = new Paragraph();
        paragraph7.setWidthPercent(100).setMargin(0).setMarginLeft(1).setPadding(0).setFixedLeading(10);
        Text text7 = new Text(referenceDebtPosition.getPaymentDetail().getCausal());
        text7.setFont(PdfPaymentNoticeManagement.getTitiilliumWebBold()).setFontSize(8);
        paragraph7.add(text7);
        cell7.add(paragraph7);
        table.addCell(cell7);

        Cell cell8 = new Cell();
        cell8.setBorder(Border.NO_BORDER).setMargin(0).setMarginTop(5).setPadding(0);
        Paragraph paragraph8 = new Paragraph();
        paragraph8.setWidthPercent(100).setMargin(0).setMarginLeft(1).setMarginTop(2).setPadding(0).setFixedLeading(8);
        Text text8 = new Text(PaymentNoticeConstants.PDF_TEXT_BANK_CBILL_CODE);
        text8.setFont(PdfPaymentNoticeManagement.getTitilliumWebRegular()).setFontSize(8);
        paragraph8.add(text8);
        cell8.add(paragraph8);
        table.addCell(cell8);

        Cell cell9 = new Cell(1, 2);
        cell9.setBorder(Border.NO_BORDER).setMargin(0).setMarginTop(5).setPadding(0);
        Paragraph paragraph9 = new Paragraph();
        paragraph9.setWidthPercent(100).setMargin(0).setMarginTop(2).setPadding(0).setPaddingLeft(10)
                .setFixedLeading(8);
        Text text9 = new Text(PaymentNoticeConstants.PDF_TEXT_BANK_NOTICE_NUMBER);
        text9.setFont(PdfPaymentNoticeManagement.getTitilliumWebRegular()).setFontSize(8);
        paragraph9.add(text9);
        cell9.add(paragraph9);
        table.addCell(cell9);

        Cell cell10 = new Cell();
        cell10.setBorder(Border.NO_BORDER).setMargin(0).setMarginTop(5).setPadding(0);
        Paragraph paragraph10 = new Paragraph();
        paragraph10.setWidthPercent(100).setMargin(0).setMarginTop(2).setPadding(0).setPaddingRight(5)
                .setTextAlignment(TextAlignment.RIGHT).setFixedLeading(8);
        Text text10 = new Text(PaymentNoticeConstants.PDF_TEXT_BANK_CI_FISCAL_CODE);
        text10.setFont(PdfPaymentNoticeManagement.getTitilliumWebRegular()).setFontSize(8);
        paragraph10.add(text10);
        cell10.add(paragraph10);
        table.addCell(cell10);

        Cell cell11 = new Cell();
        cell11.setBorder(Border.NO_BORDER).setMargin(0).setPadding(0);
        Paragraph paragraph11 = new Paragraph();
        paragraph11.setWidthPercent(100).setMargin(0).setMarginLeft(1).setPadding(0).setFixedLeading(8);
        Text text11 = new Text(paymentNotice.getCreditorInstitution().getCbillCode() != null
                ? paymentNotice.getCreditorInstitution().getCbillCode() : " ");
        text11.setFont(PdfPaymentNoticeManagement.getTitiilliumWebBold()).setFontSize(8);
        paragraph11.add(text11);
        cell11.add(paragraph11);
        table.addCell(cell11);

        Cell cell12 = new Cell(1, 2);
        cell12.setBorder(Border.NO_BORDER).setMargin(0).setPadding(0);
        Paragraph paragraph12 = new Paragraph();
        paragraph12.setWidthPercent(100).setMargin(0).setPadding(0).setPaddingLeft(10).setFixedLeading(8);
        Text text12 = new Text(PaymentNoticeBusiness
                .getFormattedNoticeNumber(referenceDebtPosition.getPaymentDetail().getNoticeNumber()));
        text12.setFont(PdfPaymentNoticeManagement.getTitiilliumWebBold()).setFontSize(8);
        paragraph12.add(text12);
        cell12.add(paragraph12);
        table.addCell(cell12);

        Cell cell13 = new Cell();
        cell13.setBorder(Border.NO_BORDER).setMargin(0).setPadding(0);
        Paragraph paragraph13 = new Paragraph();
        paragraph13.setWidthPercent(100).setMargin(0).setPadding(0).setPaddingRight(5).setFixedLeading(8)
                .setTextAlignment(TextAlignment.RIGHT);
        Text text13 = new Text(paymentNotice.getCreditorInstitution().getFiscalCode());
        text13.setFont(PdfPaymentNoticeManagement.getTitiilliumWebBold()).setFontSize(8);
        paragraph13.add(text13);
        cell13.add(paragraph13);
        table.addCell(cell13);

        table.addCell(PdfPaymentNoticeManagement.getEmptyCell(60));
        table.addCell(PdfPaymentNoticeManagement.getEmptyCell(35));
        table.addCell(PdfPaymentNoticeManagement.getEmptyCell(120));
        table.addCell(PdfPaymentNoticeManagement.getEmptyCell(100));

        return table;
    }
}
