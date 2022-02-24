package it.gov.pagopa.payments.generate.paymentnotice.constants;

/**
 * All the constants related to pdf of the Payment Notice
 */
public class PaymentNoticeConstants {

    private PaymentNoticeConstants() {
	throw new IllegalStateException("Utility class");
    }

    public static final String BASE_TEMPLATE = "/templates/pdfPaymentNotice/baseTemplate.png";
    public static final String BASE_TEMPLATE_NO_POSTAL_SECTION_NO_SINGLE_INSTALLMENT = "/templates/pdfPaymentNotice/baseTemplate_noPS_noSI.png";
    public static final String BASE_TEMPLATE_NO_POSTAL_SECTION = "/templates/pdfPaymentNotice/baseTemplate_noPS.png";
    public static final String TWO_INSTALLMENTS_TEMPLATE = "/templates/pdfPaymentNotice/twoInstallmentsTemplate.png";
    public static final String TWO_INSTALLMENTS_TEMPLATE_NO_POSTAL_SECTION = "/templates/pdfPaymentNotice/twoInstallmentsTemplate_noPS.png";
    public static final String THREE_INSTALLMENTS_TEMPLATE = "/templates/pdfPaymentNotice/threeInstallmentsTemplate.png";
    public static final String THREE_INSTALLMENTS_TEMPLATE_NO_POSTAL_SECTION = "/templates/pdfPaymentNotice/threeInstallmentsTemplate_noPS.png";

    public static final String ROBOTOFONTREGULAR = "/fonts/RobotoMono-Regular.ttf";
    public static final String ROBOTOFONTBOLD = "/fonts/RobotoMono-Bold.ttf";
    public static final String TITILLIUM_WEB_REGULAR = "/fonts/TitilliumWeb-Regular.ttf";
    public static final String TITILLIUM_WEB_BOLD = "/fonts/TitilliumWeb-Bold.ttf";
    public static final String TITILLIUM_WEB_BLACK = "/fonts/TitilliumWeb-Black.ttf";

    public static final String PAGE_TYPE_TWO_INSTALLMENTS = "2_INSTALLMENTS";
    public static final String PAGE_TYPE_THREE_INSTALLMENTS = "3_INSTALLMENTS";
    public static final String PDF_TEXT_NOTICE_PAYMENT = "AVVISO DI PAGAMENTO";
    public static final String PDF_TEXT_LOGO_EXTENSION = "png";
    public static final String PDF_TEXT_DESCRIPTION_PLACEHOLDER_ONE = "<nr1>";
    public static final String PDF_TEXT_DESCRIPTION_PLACEHOLDER_TWO = "<nr2>";
    public static final String PDF_TEXT_DESCRIPTION_PLACEHOLDER_THREE = "<nr3>";
    public static final String PDF_TEXT_DESCRIPTION_START = "Rate ";
    public static final String PDF_TEXT_DESCRIPTION_TWO_INSTALLMENTS = "nr°" + PDF_TEXT_DESCRIPTION_PLACEHOLDER_ONE
	    + " e nr°" + PDF_TEXT_DESCRIPTION_PLACEHOLDER_TWO + "";
    public static final String PDF_TEXT_DESCRIPTION_THREE_INSTALLMENTS = "nr°" + PDF_TEXT_DESCRIPTION_PLACEHOLDER_ONE
	    + ", nr°" + PDF_TEXT_DESCRIPTION_PLACEHOLDER_TWO + " e nr°" + PDF_TEXT_DESCRIPTION_PLACEHOLDER_THREE;
    public static final String PDF_TEXT_DESCRIPTION_SEPARATOR = " - ";
    public static final String PDF_TEXT_PAYMENT_CHANNEL = "Utilizza la porzione di avviso relativa alla rata ed al canale di pagamento che preferisci.";
    public static final String PDF_TEXT_CREDITOR_INSTITUTION = "ENTE CREDITORE   ";
    public static final String PDF_TEXT_FISCAL_CODE = "      Cod. Fiscale   ";
    public static final String PDF_TEXT_RECIPIENT_NOTICE = "DESTINATARIO AVVISO   ";
    public static final String PDF_TEXT_HOWMUCH_WHEN_TOPAY = "QUANTO E QUANDO PAGARE ?   ";
    public static final String PDF_TEXT_OPTIONAL_INSTALLMENT_PAYMENT = "    Puoi pagare anche a rate";
    public static final String PDF_TEXT_WHERE_TOPAY = "DOVE PAGARE ? ";
    public static final String PDF_TEXT_PAYMENT_CHANNELS_LIST = "Lista dei canali di pagamento su ";
    public static final String PDF_TEXT_PAGOPA_WEBSITE = "www.pagopa.it";
    public static final String PDF_TEXT_WEBSITE_TOPAY = "PAGA CON L'APP IO";
    public static final String PDF_TEXT_EURO = " Euro ";
    public static final String PDF_TEXT_WITHIN = " entro il ";
    public static final String PDF_TEXT_AMOUNT_FORMAT = "%,.2f";
    public static final String PDF_TEXT_EXPIRATION_DATE_FORMAT = "dd/MM/yyyy";
    public static final String PDF_TEXT_YOU_CAN_PAY = "\r \rPuoi pagare";
    public static final String PDF_TEXT_WITH = " con";
    public static final String PDF_TEXT_SINGLE_INSTALLMENT = " una unica rata";
    public static final String PDF_TEXT_POINT = ".";
    public static final String PDF_TEXT_OR = " oppure";
    public static final String PDF_TEXT_DEBT_POSITION_LIST_SIZE_PLACEHOLDER = "<debtPositionListSize>";
    public static final String PDF_TEXT_INSTALLMENT_PAYMENT_INFO = " in " + PDF_TEXT_DEBT_POSITION_LIST_SIZE_PLACEHOLDER
	    + " rate (vedi pagina seguente).\rLa rateizzazione non prevede costi aggiuntivi.";
    public static final String PDF_TEXT_AMOUNT_INFO_PART1 = "In fase di pagamento, se previsto dall’ente, l'importo potrebbe\r";
    public static final String PDF_TEXT_AMOUNT_INFO_PART2 = "essere aggiornato automaticamente e subire variazioni in diminuzione\r";
    public static final String PDF_TEXT_AMOUNT_INFO_PART3 = "(per sgravi, note di credito) o in aumento ";
    public static final String PDF_TEXT_AMOUNT_INFO_PART4 = "o in aumento (per sanzioni, interessi, ecc.).";

    public static final String PDF_TEXT_YOUR_CREDITOR_INSTITUTION = "oppure utilizza i servizi digitali del tuo ente, di Poste Italiane, ";
    public static final String PDF_TEXT_CI_WEBSITE_PLACEHOLDER = "<website>";
    public static final String PDF_TEXT_CI_WEBSITE = "(" + PDF_TEXT_CI_WEBSITE_PLACEHOLDER + ")";
    public static final String PDF_TEXT_COMMA = ", ";
    public static final String PDF_TEXT_POSTE_ITALIANE = "di Poste Italiane, ";
    public static final String PDF_TEXT_PAYMENT_INFO_PART1 = "della tua banca o degli altri canali abilitati a pagoPA.";
    public static final String PDF_TEXT_PAYMENT_INFO_PART2 = "";
    public static final String PDF_TEXT_PAY_ON_TERRITORY = "PAGA SUL TERRITORIO";
    public static final String PDF_TEXT_WHERE_TOPAY_POSTAL = "\r(anche in contanti) in ufficio postale, in banca, in ricevitoria,";
    public static final String PDF_TEXT_WHERE_TOPAY_BANKING = "\r(anche in contanti) in ufficio postale, in banca, in ricevitoria,";
    public static final String PDF_TEXT_WHERE_TOPAY_PART1 = "\rdal tabaccaio, al bancomat, al supermercato.";
    public static final String PDF_TEXT_WHERE_TOPAY_PART2 = "";
    public static final String PDF_TEXT_HOW_TOPAY = "";
    public static final String PDF_TEXT_TO_THE_INSTALLMENT = "";
    public static final String PDF_TEXT_PREFERRED_PAYMENT_CHANNEL = "";
    public static final String PDF_TEXT_BANK_OTHER_CHANNEL = "DATI PER IL PAGAMENTO";
    public static final String PDF_TEXT_BANK_SINGLE_INSTALLMENT = "RATA UNICA";
    public static final String PDF_TEXT_BANK_WITHIN = "  entro il      ";
    public static final String PDF_TEXT_BANK_PAYMENT_CODES_PART1 = "Inquadra il ";
    public static final String PDF_TEXT_BANK_PAYMENT_CODES_PART2 = "codice QR";
    public static final String PDF_TEXT_BANK_PAYMENT_CODES_PART3 = " con la tua app di pagamenti o usa i dati accanto.";
    public static final String PDF_TEXT_BANK_PAYMENT_CODES_PART4 = "";
    public static final String PDF_TEXT_BANK_PAYMENT_CODES_PART5 = "";
    public static final String PDF_TEXT_BANK_RECIPIENT = "Destinatario";
    public static final String PDF_TEXT_BANK_EURO = "Euro ";
    public static final String PDF_TEXT_BANK_CREDITOR_INSTITUTION = "Ente creditore";
    public static final String PDF_TEXT_BANK_PAYMENT_SUBJECT = "Oggetto del pagamento";
    public static final String PDF_TEXT_BANK_CBILL_CODE = "Codice CBILL";
    public static final String PDF_TEXT_BANK_NOTICE_NUMBER = "Codice Avviso";
    public static final String PDF_TEXT_BANK_CI_FISCAL_CODE = "Cod.Fiscale Ente Creditore";
    public static final String PDF_TEXT_POSTAL = "BOLLETTINO POSTALE PA";
    public static final String PDF_TEXT_POSTAL_BANCO = "Banco";
    public static final String PDF_TEXT_POSTAL_POSTA = "Posta";
    public static final String PDF_TEXT_POSTAL_WITHIN = "  entro il      ";
    public static final String PDF_TEXT_POSTAL_INSTALLMENT_NUMBER = "° RATA";
    public static final String PDF_TEXT_POSTAL_SINGLE_INSTALLMENT = "RATA UNICA";
    public static final String PDF_TEXT_POSTAL_INFO_PART1 = "Bollettino Postale pagabile in tutti\r";
    public static final String PDF_TEXT_POSTAL_INFO_PART2 = "gli Uffici Postali e sui canali fisici o\r";
    public static final String PDF_TEXT_POSTAL_INFO_PART3 = "digitali abilitati di Poste Italiane e\r";
    public static final String PDF_TEXT_POSTAL_INFO_PART4 = "dell'Ente Creditore";
    public static final String PDF_TEXT_POSTAL_CC = "sul C/C n.  ";
    public static final String PDF_TEXT_POSTAL_EURO = "Euro ";
    public static final String PDF_TEXT_POSTAL_HEADED = "Intestato a";
    public static final String PDF_TEXT_POSTAL_RECIPIENT = "Destinatario";
    public static final String PDF_TEXT_POSTAL_PAYMENT_SUBJECT = "Oggetto del pagamento";
    public static final String PDF_TEXT_POSTAL_NOTICE_NUMBER = "Codice Avviso";
    public static final String PDF_TEXT_POSTAL_TYPE_LABEL = "Tipo";
    public static final String PDF_TEXT_POSTAL_CI_FISCAL_CODE = "Cod.Fiscale Ente Creditore";
    public static final String PDF_TEXT_POSTAL_TYPE_VALUE = "P1";
    public static final String PDF_TEXT_POSTAL_MATRIX_INDIRIZZAMENTO_FASE = "codfase=";
    public static final String PDF_TEXT_POSTAL_MATRIX_CODICE_FASE_ACCETTAZIONE = "NBPA";
    public static final String PDF_TEXT_POSTAL_MATRIX_SEPARATORE = ";";
    public static final String PDF_TEXT_POSTAL_MATRIX_NOTICE_NUMBER_LENGTH = "18";
    public static final String PDF_TEXT_POSTAL_MATRIX_CC_LENGTH = "12";
    public static final String PDF_TEXT_POSTAL_MATRIX_ACCOUNT_NUMBER_FORMAT = "%012d";
    public static final String PDF_TEXT_POSTAL_MATRIX_AMOUNT_LENGTH = "10";
    public static final String PDF_TEXT_POSTAL_MATRIX_AMOUNT_FORMAT = "%010d";
    public static final String PDF_TEXT_POSTAL_MATRIX_DOC_TYPE_LENGTH = "3";
    public static final String PDF_TEXT_POSTAL_MATRIX_DOC_VALUE = "896";
    public static final String PDF_TEXT_POSTAL_MATRIX_ID = "1";
    public static final String PDF_TEXT_POSTAL_MATRIX_PAYMENT_PHASE = "P1";
    public static final String PDF_TEXT_POSTAL_MATRIX_IDENTIFICATION_CODE_FORMAT = "%1$-16s";
    public static final String PDF_TEXT_POSTAL_MATRIX_PAYER_REGISTRY_FORMAT = "%1$-40s";
    public static final String PDF_TEXT_POSTAL_MATRIX_PAYMENT_SUBJECT_FORMAT = "%1$-110s";
    public static final String PDF_TEXT_POSTAL_MATRIX_FILLER_FORMAT = "%1$-12s";
    public static final String PDF_TEXT_POSTAL_MATRIX_FILLER_VALUE = " ";
    public static final String PDF_TEXT_POSTAL_MATRIX_FINAL_VALUE = "A";
    public static final String PDF_TEXT_BANK_INST_INSTALLMENT_NUMBER = "° RATA";
    public static final String PDF_TEXT_BANK_INST_WITHIN = " entro il ";
    public static final String PDF_TEXT_BANK_INST_EURO = "Euro\r";
    public static final String PDF_TEXT_BANK_INST_CREDITOR_INSTITUTION = "Ente creditore\r";
    public static final String PDF_TEXT_BANK_INST_PAYMENT_SUBJECT = "Oggetto del pagamento\r";
    public static final String PDF_TEXT_BANK_INST_CBILL_CODE = "Codice CBILL\r";
    public static final String PDF_TEXT_BANK_INST_PCI_FISCAL_CODE = "Cod.Fiscale Ente Creditore\r";
    public static final String PDF_TEXT_BANK_INST_NOTICE_NUMBER = "Codice Avviso\r";
    public static final String PDF_TEXT_BANK_INST_3_BANK_INFO_PART1 = "Inquadra il ";
    public static final String PDF_TEXT_BANK_INST_3_BANK_INFO_PART2 = "codice QR";
    public static final String PDF_TEXT_BANK_INST_3_BANK_INFO_PART3 = " con la tua app di pagamenti o usa i dati sopra.";
    public static final String PDF_TEXT_BANK_INST_3_BANK_INFO_PART4 = "";
    public static final String PDF_TEXT_BANK_INST_3_BANK_INFO_PART5 = "";
    public static final String PDF_TEXT_BANK_INST_2_BANK_INFO_PART1 = "";
    public static final String PDF_TEXT_BANK_INST_2_BANK_INFO_PART2 = "";
    public static final String PDF_TEXT_BANK_INST_2_BANK_INFO_PART3 = "";
    public static final String PDF_TEXT_BANK_INST_2_BANK_INFO_PART4 = "";
    public static final String PDF_TEXT_BANK_INST_2_BANK_INFO_PART5 = "";
}
