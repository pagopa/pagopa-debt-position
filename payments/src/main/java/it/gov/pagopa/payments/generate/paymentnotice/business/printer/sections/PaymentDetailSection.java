package it.gov.pagopa.payments.generate.paymentnotice.business.printer.sections;

import java.io.IOException;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Locale;

import com.itextpdf.layout.border.Border;
import com.itextpdf.layout.element.Cell;
import com.itextpdf.layout.element.Paragraph;
import com.itextpdf.layout.element.Table;
import com.itextpdf.layout.element.Text;
import com.itextpdf.layout.property.TextAlignment;
import com.itextpdf.layout.property.VerticalAlignment;

import it.gov.pagopa.payments.generate.paymentnotice.bean.PaymentNotice;
import it.gov.pagopa.payments.generate.paymentnotice.business.PaymentNoticeBusiness;
import it.gov.pagopa.payments.generate.paymentnotice.business.printer.PdfPaymentNoticeManagement;
import it.gov.pagopa.payments.generate.paymentnotice.constants.PaymentNoticeConstants;

/**
 * Generates the information section of the pdf containing the payment amount,
 * the payment expiration date and the data on where to pay
 */
public class PaymentDetailSection {

    PaymentNotice paymentNotice = null;

    /**
     * Initialization of <code>PaymentDetailSection</code>
     * 
     * @param paymentNotice
     */
    public PaymentDetailSection(PaymentNotice paymentNotice) {
        super();
        this.paymentNotice = paymentNotice;
    }

    /**
     * Generates the heading of <code>PaymentDetailSection</code>
     * 
     * @return
     * @throws IOException 
     */
    public Table createFirstRow() throws IOException {
        float[] colWidths = { 273f, 4f, 273f };
        Table table = new Table(colWidths);
        table.setHeight(25).setMarginLeft(10).setMarginTop(2);

        Cell cell1 = new Cell();
        cell1.setBorder(Border.NO_BORDER);
        Paragraph paragraph1 = new Paragraph();
        paragraph1.setWidthPercent(100).setMarginTop(0).setMarginLeft(10).setFixedLeading(10);
        Text text1a1 = new Text(PaymentNoticeConstants.PDF_TEXT_HOWMUCH_WHEN_TOPAY);
        text1a1.setFont(PdfPaymentNoticeManagement.getTitiilliumWebBold()).setFontSize(9);
        paragraph1.add(text1a1);
        if (paymentNotice.getDebtPositionList().size() > 1
                && PaymentNoticeBusiness.hasSingleInstallment(paymentNotice)) {
            Text text1a2 = new Text(PaymentNoticeConstants.PDF_TEXT_OPTIONAL_INSTALLMENT_PAYMENT);
            text1a2.setFont(PdfPaymentNoticeManagement.getTitilliumWebRegular()).setFontSize(8f);
            paragraph1.add(text1a2);
        }
        cell1.add(paragraph1);
        table.addCell(cell1);

        table.addCell(PdfPaymentNoticeManagement.getEmptyCell(4));

        Cell cell2 = new Cell();
        cell2.setBorder(Border.NO_BORDER);
        Paragraph paragraph2 = new Paragraph();
        paragraph2.setWidthPercent(100).setMarginTop(0).setMarginLeft(10).setFixedLeading(10);
        Text text2a1 = new Text(PaymentNoticeConstants.PDF_TEXT_WHERE_TOPAY);
        text2a1.setFont(PdfPaymentNoticeManagement.getTitiilliumWebBold()).setFontSize(9);
        paragraph2.add(text2a1);
        Text text2a2 = new Text(PaymentNoticeConstants.PDF_TEXT_PAYMENT_CHANNELS_LIST);
        text2a2.setFont(PdfPaymentNoticeManagement.getTitilliumWebRegular()).setFontSize(8f);
        paragraph2.add(text2a2);
        Text text2a3 = new Text(PaymentNoticeConstants.PDF_TEXT_PAGOPA_WEBSITE);
        text2a3.setFont(PdfPaymentNoticeManagement.getTitiilliumWebBold()).setFontSize(8f);
        paragraph2.add(text2a3);
        cell2.add(paragraph2);
        table.addCell(cell2);

        return table;
    }

    /**
     * Generates the first information part of <code>PaymentDetailSection</code>
     * 
     * @return
     * @throws IOException 
     */
    public Table createSecondRow() throws IOException {
        float[] colWidths = { 260, 4, 280 };
        Table table = new Table(colWidths);
        table.setHeight(115).setMarginLeft(0).setMarginTop(6);

        Cell cell1 = new Cell();
        cell1.setBorder(Border.NO_BORDER);
        Paragraph paragraph1 = new Paragraph();
        paragraph1.setWidthPercent(100).setMarginTop(8).setMarginBottom(0).setWidth(260).setMarginLeft(20)
                .setFixedLeading(10).setTextAlignment(TextAlignment.LEFT);
        createAmountExpirationRow(paragraph1);
        createAmountInfoPart1(paragraph1);
        cell1.add(paragraph1);
        table.addCell(cell1);

        table.addCell(PdfPaymentNoticeManagement.getEmptyCell(4));

        Cell cell2 = new Cell();
        cell2.setBorder(Border.NO_BORDER);
        Paragraph paragraph2 = new Paragraph();
        paragraph2.setWidth(165).setMarginTop(5).setMarginLeft(10).setFixedLeading(10);
        createWhereToPayInfoPart1(paragraph2);
        cell2.add(paragraph2);
        table.addCell(cell2);

        Cell cell3 = new Cell();
        cell3.setBorder(Border.NO_BORDER);
        Paragraph paragraph3 = new Paragraph();
        paragraph3.setWidth(260).setMarginTop(2).setMarginLeft(20).setFixedLeading(8);
        createAmountInfoPart2(paragraph3);
        cell3.add(paragraph3);
        table.addCell(cell3);

        table.addCell(PdfPaymentNoticeManagement.getEmptyCell(4));

        Cell cell4 = new Cell();
        cell4.setBorder(Border.NO_BORDER);
        Paragraph paragraph4 = new Paragraph();
        paragraph4.setWidth(180).setMarginTop(2).setMarginLeft(10).setFixedLeading(10);
        createWhereToPayInfoPart2(paragraph4);
        cell4.add(paragraph4);
        table.addCell(cell4);

        return table;
    }

    /**
     * Generates the second information part of
     * <code>PaymentDetailSection</code>
     * 
     * @return
     * @throws IOException 
     */
    public Table createThirdRow() throws IOException {
        float[] colWidths = { 500, 50 };
        Table table = new Table(colWidths);
        table.setHeight(22).setMarginLeft(0).setMarginTop(5).setBorder(Border.NO_BORDER);

        Cell cell1 = new Cell();
        cell1.setBorder(Border.NO_BORDER).setMargin(0).setPadding(0).setVerticalAlignment(VerticalAlignment.BOTTOM);
        Paragraph paragraph1 = new Paragraph();
        paragraph1.setTextAlignment(TextAlignment.LEFT).setFixedLeading(11).setMargin(0).setPadding(0).setMarginLeft(20)
                .setVerticalAlignment(VerticalAlignment.BOTTOM);
        createHowToPayInfo(paragraph1);
        cell1.add(paragraph1);
        table.addCell(cell1);

        table.addCell(PdfPaymentNoticeManagement.getEmptyCell(50));

        return table;
    }

    /**
     * @param paragraph
     * @return
     * @throws IOException 
     */
    private void createAmountExpirationRow(Paragraph paragraph) throws IOException {
        Text text1 = new Text(String.format(Locale.ITALY, PaymentNoticeConstants.PDF_TEXT_AMOUNT_FORMAT,
                PaymentNoticeBusiness.getPaymentTotaleAmount(paymentNotice.getDebtPositionList())));
        text1.setFont(PdfPaymentNoticeManagement.getTitiilliumWebBold()).setFontSize(14f);
        paragraph.add(text1);

        Text text2 = new Text(PaymentNoticeConstants.PDF_TEXT_EURO);
        text2.setFont(PdfPaymentNoticeManagement.getTitiilliumWebBold()).setFontSize(14f);
        paragraph.add(text2);

        Date expirationDate = PaymentNoticeBusiness.getExpirationDate(paymentNotice.getDebtPositionList());
        if (expirationDate != null) {
            Text text3 = new Text(PaymentNoticeConstants.PDF_TEXT_WITHIN);
            text3.setFont(PdfPaymentNoticeManagement.getTitiilliumWebBold()).setFontSize(10f);
            paragraph.add(text3);

            DateFormat formatter = new SimpleDateFormat(PaymentNoticeConstants.PDF_TEXT_EXPIRATION_DATE_FORMAT);
            Text text4 = new Text(formatter.format(expirationDate));
            text4.setFont(PdfPaymentNoticeManagement.getTitiilliumWebBold()).setFontSize(14f);
            paragraph.add(text4);
        }
    }

    /**
     * @param paragraph
     * @throws IOException 
     */
    private void createAmountInfoPart1(Paragraph paragraph) throws IOException {
        Text text1a1 = new Text(PaymentNoticeConstants.PDF_TEXT_YOU_CAN_PAY);
        text1a1.setFont(PdfPaymentNoticeManagement.getTitilliumWebRegular()).setFontSize(8);
        paragraph.add(text1a1);
        if (paymentNotice.getDebtPositionList().size() == 1
                || PaymentNoticeBusiness.hasSingleInstallment(paymentNotice)) {
            Text text1a2 = new Text(PaymentNoticeConstants.PDF_TEXT_WITH);
            text1a2.setFont(PdfPaymentNoticeManagement.getTitilliumWebRegular()).setFontSize(8);
            paragraph.add(text1a2);
            Text text1a3 = new Text(PaymentNoticeConstants.PDF_TEXT_SINGLE_INSTALLMENT);
            text1a3.setFont(PdfPaymentNoticeManagement.getTitiilliumWebBold()).setFontSize(8);
            paragraph.add(text1a3);
        }
        if (paymentNotice.getDebtPositionList().size() == 1) {
            Text text1a4 = new Text(PaymentNoticeConstants.PDF_TEXT_POINT);
            text1a4.setFont(PdfPaymentNoticeManagement.getTitilliumWebRegular()).setFontSize(8);
            paragraph.add(text1a4);
        } else {
            if (PaymentNoticeBusiness.hasSingleInstallment(paymentNotice)) {
                Text text1a5 = new Text(PaymentNoticeConstants.PDF_TEXT_OR);
                text1a5.setFont(PdfPaymentNoticeManagement.getTitilliumWebRegular()).setFontSize(8);
                paragraph.add(text1a5);
            }
            Text text1a6 = new Text(PaymentNoticeConstants.PDF_TEXT_INSTALLMENT_PAYMENT_INFO.replace(
                    PaymentNoticeConstants.PDF_TEXT_DEBT_POSITION_LIST_SIZE_PLACEHOLDER,
                    String.valueOf(paymentNotice.getDebtPositionList().size()-1)));
            text1a6.setFont(PdfPaymentNoticeManagement.getTitilliumWebRegular()).setFontSize(8);
            paragraph.add(text1a6);
        }
    }

    /**
     * @param paragraph
     * @throws IOException 
     */
    private void createAmountInfoPart2(Paragraph paragraph) throws IOException {
        StringBuilder sb = new StringBuilder(PaymentNoticeConstants.PDF_TEXT_AMOUNT_INFO_PART1);
        sb.append(PaymentNoticeConstants.PDF_TEXT_AMOUNT_INFO_PART2);
        sb.append(PaymentNoticeConstants.PDF_TEXT_AMOUNT_INFO_PART3);
        sb.append(PaymentNoticeConstants.PDF_TEXT_AMOUNT_INFO_PART4);
        Text text = new Text(sb.toString());
        text.setFont(PdfPaymentNoticeManagement.getTitilliumWebRegular()).setFontSize(8);
        paragraph.add(text);
    }

    /**
     * @param paragraph
     * @throws IOException 
     */
    private void createWhereToPayInfoPart1(Paragraph paragraph) throws IOException {
        Text text1 = new Text(PaymentNoticeConstants.PDF_TEXT_WEBSITE_TOPAY);
        text1.setFont(PdfPaymentNoticeManagement.getTitiilliumWebBold()).setFontSize(10f);
        paragraph.add(text1);
        StringBuilder sb = new StringBuilder("\r");
        if (paymentNotice.getIsModello1or2().booleanValue()) {
            sb.append(PaymentNoticeConstants.PDF_TEXT_YOUR_CREDITOR_INSTITUTION);
        }
        if (paymentNotice.getCreditorInstitution().getPostalAuthorizationCode() != null)
            sb.append(PaymentNoticeConstants.PDF_TEXT_POSTE_ITALIANE);
        sb.append(PaymentNoticeConstants.PDF_TEXT_PAYMENT_INFO_PART1);
        sb.append(PaymentNoticeConstants.PDF_TEXT_PAYMENT_INFO_PART2);
        if (paymentNotice.getCreditorInstitution().getPostalAuthorizationCode() != null)
            sb.append("\r");
        Text text2 = new Text(sb.toString());
        text2.setFont(PdfPaymentNoticeManagement.getTitilliumWebRegular()).setFontSize(6);
        paragraph.add(text2);
    }

    /**
     * @param paragraph
     * @throws IOException 
     */
    private void createWhereToPayInfoPart2(Paragraph paragraph) throws IOException {
        Text text1 = new Text(PaymentNoticeConstants.PDF_TEXT_PAY_ON_TERRITORY);
        text1.setFont(PdfPaymentNoticeManagement.getTitiilliumWebBold()).setFontSize(11f);
        paragraph.add(text1);
        String sbText = ((paymentNotice.getCreditorInstitution().getPostalAuthorizationCode() != null)
                ? PaymentNoticeConstants.PDF_TEXT_WHERE_TOPAY_POSTAL
                : PaymentNoticeConstants.PDF_TEXT_WHERE_TOPAY_BANKING);
        StringBuilder sb = new StringBuilder(sbText);
        sb.append(PaymentNoticeConstants.PDF_TEXT_WHERE_TOPAY_PART1);
        sb.append(PaymentNoticeConstants.PDF_TEXT_WHERE_TOPAY_PART2);
        Text text2 = new Text(sb.toString());
        text2.setFont(PdfPaymentNoticeManagement.getTitilliumWebRegular()).setFontSize(6);
        paragraph.add(text2);
    }

    /**
     * @param paragraph
     * @throws IOException 
     */
    private void createHowToPayInfo(Paragraph paragraph) throws IOException {
        Text text1 = new Text(PaymentNoticeConstants.PDF_TEXT_HOW_TOPAY);
        text1.setFont(PdfPaymentNoticeManagement.getTitilliumWebRegular()).setFontSize(11);
        paragraph.add(text1);
        String text2a1 = ((paymentNotice.getDebtPositionList().size() > 1
                && PaymentNoticeBusiness.hasSingleInstallment(paymentNotice))
                        ? PaymentNoticeConstants.PDF_TEXT_TO_THE_INSTALLMENT : "")
                + PaymentNoticeConstants.PDF_TEXT_PREFERRED_PAYMENT_CHANNEL;
        Text text2a2 = new Text(text2a1);
        text2a2.setFont(PdfPaymentNoticeManagement.getTitilliumWebRegular()).setFontSize(11);
        paragraph.add(text2a2);
    }
}
