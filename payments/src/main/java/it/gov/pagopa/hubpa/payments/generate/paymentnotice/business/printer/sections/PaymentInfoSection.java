package it.gov.pagopa.hubpa.payments.generate.paymentnotice.business.printer.sections;

import java.io.IOException;

import com.itextpdf.layout.border.Border;
import com.itextpdf.layout.element.Cell;
import com.itextpdf.layout.element.Paragraph;
import com.itextpdf.layout.element.Table;
import com.itextpdf.layout.element.Text;

import it.gov.pagopa.hubpa.payments.generate.debtposition.bean.DebtPosition;
import it.gov.pagopa.hubpa.payments.generate.paymentnotice.bean.PaymentNotice;
import it.gov.pagopa.hubpa.payments.generate.paymentnotice.business.printer.PdfPaymentNoticeManagement;
import it.gov.pagopa.hubpa.payments.generate.paymentnotice.constants.PaymentNoticeConstants;

/**
 * Generates the information section of the pdf containing the details of the
 * Creditor Institution and the Payer
 */
public class PaymentInfoSection {

    PaymentNotice paymentNotice = null;
    DebtPosition referenceDebtPosition = null;

    /**
     * Initialization of <code>PaymentInfoSection</code>
     * 
     * @param paymentNotice
     * @see PaymentNotice
     */
    public PaymentInfoSection(PaymentNotice paymentNotice) {
        super();
        this.paymentNotice = paymentNotice;
        this.referenceDebtPosition = paymentNotice.getDebtPositionList().get(0);
    }

    /**
     * Generates the heading of <code>PaymentInfoSection</code>
     * 
     * @return
     * @throws IOException 
     */
    public Table createFirstRow() throws IOException {
        float[] colWidths = { 276f, 4f, 273f };
        Table table = new Table(colWidths);
        table.setHeight(23).setMarginLeft(10).setMarginTop(4).setPadding(0).setBorder(Border.NO_BORDER);

        Cell cell1 = new Cell();
        cell1.setBorder(Border.NO_BORDER).setPadding(0);
        Paragraph paragraph1 = new Paragraph();
        paragraph1.setWidthPercent(100).setPadding(0).setMarginTop(0).setMarginLeft(10).setFixedLeading(10);
        Text text1 = new Text(PaymentNoticeConstants.PDF_TEXT_CREDITOR_INSTITUTION);
        text1.setFont(PdfPaymentNoticeManagement.getTitiilliumWebBold()).setFontSize(9);
        paragraph1.add(text1);
        Text text2 = new Text(PaymentNoticeConstants.PDF_TEXT_FISCAL_CODE);
        text2.setFont(PdfPaymentNoticeManagement.getTitilliumWebRegular()).setFontSize(9);
        paragraph1.add(text2);
        Text text3 = new Text(paymentNotice.getCreditorInstitution().getFiscalCode());
        text3.setFont(PdfPaymentNoticeManagement.getRobotoFontRegular()).setFontSize(9);
        paragraph1.add(text3);
        cell1.add(paragraph1);
        table.addCell(cell1);

        table.addCell(PdfPaymentNoticeManagement.getEmptyCell(4));

        Cell cell2 = new Cell();
        cell2.setBorder(Border.NO_BORDER).setPadding(0);
        Paragraph paragraph2 = new Paragraph();
        paragraph2.setWidthPercent(100).setMarginTop(0).setMarginLeft(10).setPadding(0).setFixedLeading(10);
        Text text4 = new Text(PaymentNoticeConstants.PDF_TEXT_RECIPIENT_NOTICE);
        text4.setFont(PdfPaymentNoticeManagement.getTitiilliumWebBold()).setFontSize(9);
        paragraph2.add(text4);
        Text text5 = new Text(PaymentNoticeConstants.PDF_TEXT_FISCAL_CODE);
        text5.setFont(PdfPaymentNoticeManagement.getTitilliumWebRegular()).setFontSize(9);
        paragraph2.add(text5);
        Text text6 = new Text(referenceDebtPosition.getPayer().getUniqueIdentificationCode());
        text6.setFont(PdfPaymentNoticeManagement.getRobotoFontRegular()).setFontSize(9);
        paragraph2.add(text6);
        cell2.add(paragraph2);
        table.addCell(cell2);

        return table;
    }

    /**
     * Generates the first information row of <code>PaymentInfoSection</code>
     * 
     * @return
     * @throws IOException 
     */
    public Table createSecondRow() throws IOException {
        float[] colWidths = { 273f, 4f, 273f };
        Table table = new Table(colWidths);
        table.setHeight(30).setMarginLeft(11).setMarginTop(0);

        Cell cell1 = new Cell();
        cell1.setBorder(Border.NO_BORDER);
        Paragraph paragraph1 = new Paragraph();
        paragraph1.setWidthPercent(100).setMarginTop(0).setMarginLeft(10);
        Text text1 = new Text(paymentNotice.getCreditorInstitution().getName());
        text1.setFont(PdfPaymentNoticeManagement.getTitiilliumWebBold()).setFontSize(10);
        paragraph1.add(text1);
        cell1.add(paragraph1);
        table.addCell(cell1);

        table.addCell(PdfPaymentNoticeManagement.getEmptyCell(4));

        Cell cell2 = new Cell();
        cell2.setBorder(Border.NO_BORDER);
        Paragraph paragraph2 = new Paragraph();
        paragraph2.setWidthPercent(100).setMarginTop(0).setMarginLeft(16);
        Text text2 = new Text(referenceDebtPosition.getPayer().getRegistry());
        text2.setFont(PdfPaymentNoticeManagement.getTitiilliumWebBold()).setFontSize(11);
        paragraph2.add(text2);
        cell2.add(paragraph2);
        table.addCell(cell2);

        return table;
    }

    /**
     * Generates the second information row of <code>PaymentInfoSection</code>
     * 
     * @return
     * @throws IOException 
     */
    public Table createThirdRow() throws IOException {
        float[] colWidths = { 273f, 4f, 273f };
        Table table = new Table(colWidths);
        table.setHeight(28).setMarginLeft(10).setMarginTop(0);

        Cell cell1 = new Cell();
        cell1.setBorder(Border.NO_BORDER);
        Paragraph paragraph1 = new Paragraph();
        paragraph1.setWidthPercent(100).setMarginTop(0).setMarginLeft(10);
        Text text1 = new Text(paymentNotice.getCreditorInstitution().getSector());
        text1.setFont(PdfPaymentNoticeManagement.getTitilliumWebRegular()).setFontSize(10);
        paragraph1.add(text1);
        cell1.add(paragraph1);
        table.addCell(cell1);

        table.addCell(PdfPaymentNoticeManagement.getEmptyCell(4));

        Cell cell2 = new Cell();
        cell2.setBorder(Border.NO_BORDER);
        Paragraph paragraph2 = new Paragraph();
        paragraph2.setWidthPercent(100).setMarginTop(0).setMarginLeft(16);
        Text text2 = new Text(createAddressInfoPart1());
        text2.setFont(PdfPaymentNoticeManagement.getTitilliumWebRegular()).setFontSize(10);
        paragraph2.add(text2);
        cell2.add(paragraph2);
        table.addCell(cell2);

        return table;
    }

    /**
     * Generates the third information row of <code>PaymentInfoSection</code>
     * 
     * @return
     * @throws IOException 
     */
    public Table createFourthRow() throws IOException {
        float[] colWidths = { 273f, 4f, 273f };
        Table table = new Table(colWidths);
        table.setHeight(52).setMarginLeft(10).setMarginTop(0);

        Cell cell1 = new Cell();
        cell1.setBorder(Border.NO_BORDER);
        Paragraph paragraph1 = new Paragraph();
        paragraph1.setWidthPercent(100).setMarginTop(3).setMarginLeft(10).setFixedLeading(9);
        Text text1 = new Text(createCreditorInstitutionInfo());
        text1.setFont(PdfPaymentNoticeManagement.getTitilliumWebRegular()).setFontSize(8);
        paragraph1.add(text1);
        cell1.add(paragraph1);
        table.addCell(cell1);

        table.addCell(PdfPaymentNoticeManagement.getEmptyCell(4));

        Cell cell2 = new Cell();
        cell2.setBorder(Border.NO_BORDER);
        Paragraph paragraph2 = new Paragraph();
        paragraph2.setWidthPercent(100).setMarginTop(0).setMarginLeft(16);
        Text text2 = new Text(createAddressInfoPart2());
        text2.setFont(PdfPaymentNoticeManagement.getTitilliumWebRegular()).setFontSize(10);
        paragraph2.add(text2);
        cell2.add(paragraph2);
        table.addCell(cell2);

        return table;
    }

    /**
     * @return
     */
    private String createAddressInfoPart1() {
        String address = referenceDebtPosition.getPayer().getAddress();
        String number = referenceDebtPosition.getPayer().getNumberStreet();
        return (address != null ? address : "") + (number != null ? " " + number : "");
    }

    /**
     * @return
     */
    private String createAddressInfoPart2() {
        String postalCode = referenceDebtPosition.getPayer().getPostalCode();
        String locality = referenceDebtPosition.getPayer().getLocality();
        String province = referenceDebtPosition.getPayer().getProvince();
        return (postalCode != null ? postalCode : "") + (locality != null ? " " + locality : "")
                + (province != null ? " " + province : "");
    }

    /**
     * @return
     */
    private String createCreditorInstitutionInfo() {
        return paymentNotice.getCreditorInstitution().getInfo() + "\r\r\r"
                + (paymentNotice.getCreditorInstitution().getWebsite() != null
                        ? paymentNotice.getCreditorInstitution().getWebsite() : "");
    }
}
