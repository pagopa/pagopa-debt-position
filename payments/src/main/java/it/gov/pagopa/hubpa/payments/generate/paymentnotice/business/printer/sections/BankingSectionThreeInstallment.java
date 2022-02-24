package it.gov.pagopa.hubpa.payments.generate.paymentnotice.business.printer.sections;

import java.io.IOException;
import java.util.HashMap;

import com.itextpdf.kernel.pdf.PdfDocument;
import com.itextpdf.layout.border.Border;
import com.itextpdf.layout.element.Cell;
import com.itextpdf.layout.element.Paragraph;
import com.itextpdf.layout.element.Table;
import com.itextpdf.layout.element.Text;

import it.gov.pagopa.hubpa.payments.generate.debtposition.bean.DebtPosition;
import it.gov.pagopa.hubpa.payments.generate.paymentnotice.bean.PaymentNotice;
import it.gov.pagopa.hubpa.payments.generate.paymentnotice.business.printer.BankingSectionInstallmentsBusiness;
import it.gov.pagopa.hubpa.payments.generate.paymentnotice.business.printer.PdfPaymentNoticeManagement;
import it.gov.pagopa.hubpa.payments.generate.paymentnotice.constants.PaymentNoticeConstants;

/**
 * Generates the installments section of the pdf if it has 3 installments
 */
public class BankingSectionThreeInstallment {

    PaymentNotice paymentNotice = null;
    PdfDocument pdfDocument = null;
    int startingInstallmentNumber = 0;
    HashMap<Integer, DebtPosition> sortedDebtPositionHashMap = null;

    /**
     * Initialization of <code>BankingSectionThreeInstallment</code>
     * 
     * @param paymentNotice
     * @param pdfDocument
     * @param startingInstallmentNumber
     *            installmentNumber from which the subsequent number of
     *            installments are calculated
     * @param sortedDebtPositionHashMap
     *            ordered HashMap of <code>debtPosition</code>
     * @see PaymentNotice
     */
    public BankingSectionThreeInstallment(PaymentNotice paymentNotice, PdfDocument pdfDocument,
            int startingInstallmentNumber, HashMap<Integer, DebtPosition> sortedDebtPositionHashMap) {
        super();
        this.paymentNotice = paymentNotice;
        this.pdfDocument = pdfDocument;
        this.startingInstallmentNumber = startingInstallmentNumber;
        this.sortedDebtPositionHashMap = sortedDebtPositionHashMap;
    }

    /**
     * Generates the heading of <code>BankingSectionThreeInstallment</code>
     * 
     * @return
     * @throws IOException 
     */
    public Table createFirstRow() throws IOException {
        float[] colWidths = { 164, 4, 188, 4, 164 };
        Table table = new Table(colWidths);
        table.setHeight(20).setMargin(0).setMarginLeft(20).setPadding(0);
        table.addCell(BankingSectionInstallmentsBusiness.createExpirationDateCell(sortedDebtPositionHashMap,
                startingInstallmentNumber + 1));
        table.addCell(PdfPaymentNoticeManagement.getEmptyCell(4));
        table.addCell(BankingSectionInstallmentsBusiness.createExpirationDateCell(sortedDebtPositionHashMap,
                startingInstallmentNumber + 2));
        table.addCell(PdfPaymentNoticeManagement.getEmptyCell(4));
        table.addCell(BankingSectionInstallmentsBusiness.createExpirationDateCell(sortedDebtPositionHashMap,
                startingInstallmentNumber + 3));

        return table;
    }

    /**
     * Generates the information part of <code>BankingSectionThreeInstallment</code>
     * 
     * @return
     * @throws IOException 
     */
    public Table createSecondRow() throws IOException {
        float[] colWidths = { 175, 8, 175, 20, 175 };
        Table table = new Table(colWidths);
        table.setHeight(218).setMargin(0).setMarginLeft(15).setPadding(0);
        table.addCell(createPaymentInfoCell(startingInstallmentNumber + 1));
        table.addCell(PdfPaymentNoticeManagement.getEmptyCell(8));
        table.addCell(createPaymentInfoCell(startingInstallmentNumber + 2));
        table.addCell(PdfPaymentNoticeManagement.getEmptyCell(20));
        table.addCell(createPaymentInfoCell(startingInstallmentNumber + 3));

        return table;
    }

    /**
     * @param installmentNumber
     * @return
     * @throws IOException 
     */
    private Cell createPaymentInfoCell(int installmentNumber) throws IOException {
        float[] colWidths = { 55, 35, 80 };
        Table table = new Table(colWidths);
        table.setMargin(0).setPadding(0);
        Cell cell = new Cell();
        cell.setBorder(Border.NO_BORDER).setPadding(0).setMargin(0);
        DebtPosition debtPosition = sortedDebtPositionHashMap.get(installmentNumber);
        if (debtPosition != null) {
            table.addCell(BankingSectionInstallmentsBusiness.createQrCodeCell(debtPosition, pdfDocument,
                    paymentNotice.getCreditorInstitution().getFiscalCode()));
            table.addCell(BankingSectionInstallmentsBusiness.createAmountCell(debtPosition));
            table.addCell(BankingSectionInstallmentsBusiness
                    .createCreditorInstitutionCell(paymentNotice.getCreditorInstitution().getName()));
            table.addCell(BankingSectionInstallmentsBusiness.createPaymentDetailsCell(debtPosition));
            table.addCell(BankingSectionInstallmentsBusiness
                    .createCbillCell(paymentNotice.getCreditorInstitution().getCbillCode()));
            table.addCell(BankingSectionInstallmentsBusiness
                    .createFiscalCodeCell(paymentNotice.getCreditorInstitution().getFiscalCode()));
            table.addCell(BankingSectionInstallmentsBusiness.createNoticeNumberCell(debtPosition));
            table.addCell(createBankingInfoCell());
        }
        return cell.add(table);
    }

    /**
     * @return
     * @throws IOException 
     */
    private Cell createBankingInfoCell() throws IOException {
        Cell cell = new Cell(1, 3);
        cell.setBorder(Border.NO_BORDER).setMargin(0).setPadding(0);
        Paragraph paragraph = new Paragraph();
        paragraph.setWidthPercent(100).setMargin(0).setPadding(0).setFixedLeading(6);
        Text text1 = new Text(PaymentNoticeConstants.PDF_TEXT_BANK_INST_3_BANK_INFO_PART1);
        text1.setFont(PdfPaymentNoticeManagement.getTitilliumWebRegular()).setFontSize(6);
        paragraph.add(text1);
        Text text2 = new Text(PaymentNoticeConstants.PDF_TEXT_BANK_INST_3_BANK_INFO_PART2);
        text2.setFont(PdfPaymentNoticeManagement.getTitiilliumWebBold()).setFontSize(6);
        paragraph.add(text2);
        Text text3 = new Text(PaymentNoticeConstants.PDF_TEXT_BANK_INST_3_BANK_INFO_PART3);
        text3.setFont(PdfPaymentNoticeManagement.getTitilliumWebRegular()).setFontSize(6);
        paragraph.add(text3);
        Text text4 = new Text(PaymentNoticeConstants.PDF_TEXT_BANK_INST_3_BANK_INFO_PART4);
        text4.setFont(PdfPaymentNoticeManagement.getTitiilliumWebBold()).setFontSize(6);
        paragraph.add(text4);
        Text text5 = new Text(PaymentNoticeConstants.PDF_TEXT_BANK_INST_3_BANK_INFO_PART5);
        text5.setFont(PdfPaymentNoticeManagement.getTitilliumWebRegular()).setFontSize(6);
        paragraph.add(text5);
        return cell.add(paragraph);
    }
}