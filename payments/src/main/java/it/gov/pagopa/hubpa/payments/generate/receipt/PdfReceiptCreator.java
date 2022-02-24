package it.gov.pagopa.hubpa.payments.generate.receipt;

import java.io.IOException;
import java.io.InputStream;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.List;

import org.springframework.util.StreamUtils;

import com.fasterxml.jackson.core.type.TypeReference;
import com.itextpdf.io.source.ByteArrayOutputStream;
import com.itextpdf.kernel.color.Color;
import com.itextpdf.kernel.color.ColorConstants;
import com.itextpdf.kernel.color.DeviceRgb;
import com.itextpdf.kernel.geom.PageSize;
import com.itextpdf.kernel.pdf.PdfDocument;
import com.itextpdf.kernel.pdf.PdfWriter;
import com.itextpdf.layout.Document;
import com.itextpdf.layout.border.Border;
import com.itextpdf.layout.border.SolidBorder;
import com.itextpdf.layout.element.AreaBreak;
import com.itextpdf.layout.element.Cell;
import com.itextpdf.layout.element.Image;
import com.itextpdf.layout.element.Paragraph;
import com.itextpdf.layout.element.Table;
import com.itextpdf.layout.element.Text;
import com.itextpdf.layout.property.HorizontalAlignment;
import com.itextpdf.layout.property.TextAlignment;

import it.gov.pagopa.hubpa.commons.model.PaDto;
import it.gov.pagopa.hubpa.payments.entity.Debitor;
import it.gov.pagopa.hubpa.payments.entity.PaymentOptions;
import it.gov.pagopa.hubpa.payments.entity.PaymentPosition;
import it.gov.pagopa.hubpa.payments.entity.Transfers;
import it.gov.pagopa.hubpa.payments.enumeration.PaymentOptionStatusEnum;
import it.gov.pagopa.hubpa.payments.generate.paymentnotice.bean.PaymentNotice;
import it.gov.pagopa.hubpa.payments.generate.paymentnotice.business.printer.PdfPaymentNoticeManagement;
import it.gov.pagopa.hubpa.payments.generate.receipt.constants.ReceiptConstants;

/**
 * Generates the pdf of the payment notice based on <code>debtPosition</code>
 * list
 */
public class PdfReceiptCreator {

	private PdfWriter pdfWriter = null;
	private PdfDocument pdfDocument = null;
	private Document document = null;
	private ByteArrayOutputStream stream = null;
	private Color fontColorRowValue = null;
	private Color fontColorRowLabel = null;
	private Color fontColorRowBlack = null;
	Border border = null;
	int fontSizeRow = 13;
	int fontSizeSubRow = 9;
	private PaymentPosition paymentPosition = null;
	private PaDto ente = null;

	/**
	 * Public constructor
	 * 
	 * @param paymentNotice
	 * @throws Exception
	 * @see PaymentNotice
	 */
	public PdfReceiptCreator(PaymentPosition paymentPosition, PaDto ente) {
		super();
		this.paymentPosition = paymentPosition;
		this.ente = ente;
		initialize();
	}

	/**
	 * Initialization of pdf objects
	 */
	private void initialize() {
		stream = new ByteArrayOutputStream();
		pdfWriter = new PdfWriter(stream);
		pdfDocument = new PdfDocument(pdfWriter);
		PageSize pageSize = new PageSize(PageSize.A4);
		document = new Document(pdfDocument, pageSize);
		document.setMargins(20, 15, 15, 10);
		border = new SolidBorder(ColorConstants.GRAY, 1);
		fontSizeRow = 13;
		fontSizeSubRow = 9;
		fontColorRowValue = new DeviceRgb(23, 50, 77);
		fontColorRowLabel = new DeviceRgb(71, 90, 109);
		fontColorRowBlack = new DeviceRgb(23, 50, 77);
	}

	public void createDocument() throws IOException {
		Debitor debitor = paymentPosition.getDebitor();
		List<PaymentOptions> paymentOptions = paymentPosition.getPaymentOptions();

		String name = debitor.getName() + " " + debitor.getSurname();

		String paymentDate = null;
		boolean firstInstallment = true;
		for (PaymentOptions paymentOption : paymentOptions) {
			if (paymentOption.getStatus().intValue() == PaymentOptionStatusEnum.PAGATO.getStatus().intValue()) {
				if (!firstInstallment) {
					document.add(new AreaBreak());
				}
				firstInstallment = false;
				BigDecimal amount = paymentOption.getAmount();
				String reason = "";
				String notificationCode = paymentOption.getNotificationCode();
				LocalDateTime paymentDateDt = paymentOption.getPaymentDate();
				if (paymentDateDt != null) {
					paymentDate = paymentDateDt.format(DateTimeFormatter.ofPattern("dd/MM/yyyy kk:mm:ss"));
				}
				List<Transfers> transfers = paymentOption.getTransfers();
				reason = this.getReason(transfers);
				BigDecimal fee = BigDecimal.ZERO;
				if (paymentOption.getFee() != null) {
					fee = paymentOption.getFee();
				}
				String total = (amount.add(fee)).setScale(2).toPlainString();
				document.add(createHeaderSection());
				document.add(
						createTitleSection(ReceiptConstants.PDF_TEXT_TITLE_PART1 + total + ReceiptConstants.PDF_TEXT_TITLE_PART2));
				document.add(createRowSection(ReceiptConstants.PDF_TEXT_NAME, name, "", 1));
				document.add(createRowSection(ReceiptConstants.PDF_TEXT_ADDRESS, debitor.getEmail(), "", 1));
				document.add(
						createRowSection(ReceiptConstants.PDF_TEXT_TRANSACTION_MANAGER, paymentOption.getPspCompanyName(), "", 1));
				document.add(createRowSection(ReceiptConstants.PDF_TEXT_PAYMENT_DATE, paymentDate, "", 1));
				document.add(createRowSection(ReceiptConstants.PDF_TEXT_IUR, paymentOption.getReceiptId(), "", 1));
				document.add(createRowSection(ReceiptConstants.PDF_TEXT_NOTICE_NUMBER, notificationCode, "", 2));
				document
						.add(createRowSection(ReceiptConstants.PDF_TEXT_CREDITOR, ente.getDesAmm(), ente.getCodiceFiscale(), 4));
				document.add(createRowSection(ReceiptConstants.PDF_TEXT_DEBITOR, name, debitor.getFiscalCode(), 4));
				document.add(createRowSection(ReceiptConstants.PDF_TEXT_PAYMENT_SUBJECT, reason, "", 1));
				document.add(createRowSection(ReceiptConstants.PDF_TEXT_AMOUNT, amount.setScale(2).toPlainString(), "", 1));
				document
						.add(createRowSection(ReceiptConstants.PDF_TEXT_TRANSACTION_COST, fee.setScale(2).toPlainString(), "", 1));
				document.add(createRowSection(ReceiptConstants.PDF_TEXT_TOTAL_AMOUNT, total, "", 3));
			}
		}

	}

	private String getReason(List<Transfers> transfers) {
		for (Transfers transfer : transfers) {
			if (ente.getCodiceFiscale().equals(transfer.getOrganizationFiscalCode())) {
				return transfer.getReason();
			}
		}
		return "";
	}

	public Table createHeaderSection() throws IOException {
		float[] colWidths = { 400, 110 };
		Table table = new Table(colWidths);
		table.setHeight(50).setMarginLeft(50).setMarginRight(50).setPadding(0);

		Cell cell0 = new Cell(1, 1);
		cell0.setBorder(Border.NO_BORDER).setWidth(110);
		Paragraph paragraph0 = new Paragraph();
		paragraph0.setWidthPercent(100).setTextAlignment(TextAlignment.LEFT)
				.setHorizontalAlignment(HorizontalAlignment.RIGHT);

		String imageCreditorPath = ReceiptConstants.PDF_IMG_TRANSPARENT;
		InputStream is2 = TypeReference.class.getResourceAsStream(new StringBuffer(imageCreditorPath).toString());
		Image img2 = PdfPaymentNoticeManagement.getLogoImageFromByte(StreamUtils.copyToByteArray(is2), true);

		img2.setMarginRight(5);
		paragraph0.add(img2);
		cell0.add(paragraph0);
		table.addCell(cell0);

		Cell cell2 = new Cell(1, 1);
		cell2.setBorder(Border.NO_BORDER).setWidth(110);
		Paragraph paragraph2 = new Paragraph();
		paragraph2.setWidthPercent(100).setTextAlignment(TextAlignment.RIGHT)
				.setHorizontalAlignment(HorizontalAlignment.RIGHT);

		String imagePath = ReceiptConstants.PDF_IMG_PAGOPA;
		InputStream is = TypeReference.class.getResourceAsStream(new StringBuffer(imagePath).toString());
		Image img = PdfPaymentNoticeManagement.getLogoImageFromByte(StreamUtils.copyToByteArray(is), true);

		img.setMarginRight(5);
		paragraph2.add(img);
		cell2.add(paragraph2);
		table.addCell(cell2);

		return table;
	}

	public Table createTitleSection(String title) throws IOException {
		float[] colWidths = { 400, 110 };
		Table table = new Table(colWidths);
		table.setMarginLeft(50).setMarginRight(50).setMarginTop(10).setPadding(0);

		Cell cell0 = new Cell(1, 2);
		cell0.setBorder(Border.NO_BORDER).setWidth(110);
		Paragraph paragraph0 = new Paragraph();
		paragraph0.setWidthPercent(100).setTextAlignment(TextAlignment.LEFT).setFixedLeading(24)
				.setHorizontalAlignment(HorizontalAlignment.RIGHT);

		Text text1 = new Text(title);
		text1.setFont(PdfPaymentNoticeManagement.getTitiilliumWebBold()).setFontSize(26);

		paragraph0.add(text1);
		cell0.add(paragraph0);
		table.addCell(cell0);

		return table;
	}

	public Table createRowSection(String textLabel, String textValue, String textSubValue, int type) throws IOException {
		if (textLabel == null) {
			textLabel = "";
		}
		if (textValue == null) {
			textValue = "";
		}
		if (textSubValue == null) {
			textSubValue = "";
		}
		Text text1 = new Text(textLabel);
		Text text2 = new Text(textValue);
		Text text3 = new Text(textSubValue);
		if (type == 2) {
			text1 = new Text(textLabel + " " + textValue);
		}
		return getTableRow(text1, text2, text3, type);
	}

	private Table getTableRow(Text text1, Text text2, Text text3, int type) throws IOException {
		Table table = getDefaultTable(type);

		Cell cell0 = getCellBorderBottom(border, type);
		Paragraph paragraph0 = getDefaultRowParagraphLeft();

		if (type == 1 || type == 4) {
			text1.setFont(PdfPaymentNoticeManagement.getTitilliumWebRegular()).setFontSize(this.fontSizeRow)
					.setFontColor(this.fontColorRowLabel);
		} else if (type == 2) {

			paragraph0.setBackgroundColor(new DeviceRgb(236, 236, 236));

			text1.setFont(PdfPaymentNoticeManagement.getTitiilliumWebBold()).setFontSize(this.fontSizeRow)
					.setFontColor(this.fontColorRowValue);
		} else if (type == 3) {
			text1.setFont(PdfPaymentNoticeManagement.getTitiilliumWebBold()).setFontSize(this.fontSizeRow)
					.setFontColor(this.fontColorRowBlack);
		}

		paragraph0.add(text1);

		cell0.add(paragraph0);
		table.addCell(cell0);

		if (type != 2) {
			Cell cell2 = getCellBorderBottom(border, type);
			Paragraph paragraph2 = getDefaultRowParagraphRight();
			if (type == 1 || type == 4) {
				text2.setFont(PdfPaymentNoticeManagement.getTitilliumWebRegular()).setFontSize(this.fontSizeRow)
						.setFontColor(this.fontColorRowValue);
			} else {
				text2.setFont(PdfPaymentNoticeManagement.getTitiilliumWebBold()).setFontSize(this.fontSizeRow)
						.setFontColor(this.fontColorRowBlack);
			}
			paragraph2.add(text2);
			if (type == 4) {
				text3.setFont(PdfPaymentNoticeManagement.getTitilliumWebRegular()).setFontSize(this.fontSizeSubRow)
						.setFontColor(this.fontColorRowValue);
				paragraph2.add(new Text("\n"));
				paragraph2.add(text3);
			}
			cell2.add(paragraph2);
			table.addCell(cell2);
		}
		return table;
	}

	private Table getDefaultTable(int type) {
		float[] colWidths = { 290, 290 };
		Table table = new Table(colWidths);
		table.setMarginLeft(50).setMarginRight(50).setPadding(0).setBorder(Border.NO_BORDER);
		if (type == 1) {
			table.setHeight(35).setMarginTop(10);
		} else if (type == 2) {
			table.setHeight(28).setMarginTop(20);
		}
		return table;
	}

	private Cell getCellBorderBottom(Border border, int type) {
		Cell cell0 = null;
		if (type != 2) {
			cell0 = new Cell(1, 1);
			cell0.setBorder(Border.NO_BORDER).setBorderBottom(border).setWidth(110);
		} else {
			cell0 = new Cell(1, 2);
			cell0.setBorder(Border.NO_BORDER).setWidth(110);
		}

		return cell0;
	}

	private Paragraph getDefaultRowParagraphRight() {
		Paragraph paragraph2 = getDefualtRowParagraph();
		paragraph2.setTextAlignment(TextAlignment.RIGHT);
		return paragraph2;
	}

	private Paragraph getDefaultRowParagraphLeft() {
		Paragraph paragraph2 = getDefualtRowParagraph();
		paragraph2.setTextAlignment(TextAlignment.LEFT);
		return paragraph2;
	}

	private Paragraph getDefualtRowParagraph() {
		Paragraph paragraph2 = new Paragraph();
		paragraph2.setWidthPercent(100).setFixedLeading(15).setHorizontalAlignment(HorizontalAlignment.RIGHT);
		return paragraph2;
	}

	/**
	 * @throws IOException
	 * @throws Exception
	 */
	public void closeStreamsAndDocument() throws IOException {

		if (pdfWriter != null && !pdfWriter.isCloseStream()) {
			pdfWriter.close();
		}

		if (pdfDocument != null && !pdfDocument.isClosed()) {
			pdfDocument.close();
		}

		if (document != null) {
			document.close();
		}

		if (stream != null) {
			stream.close();
		}
	}

	/**
	 * @return byteArray of the pdf of notice payment
	 */
	public byte[] getDocumentInBytes() {
		return stream.toByteArray();
	}
}
