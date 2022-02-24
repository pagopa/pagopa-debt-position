package it.gov.pagopa.payments.generate.receipt.constants;

/**
 * All the constants related to pdf of the Payment Notice
 */
public class ReceiptConstants {

    private ReceiptConstants() {
	throw new IllegalStateException("Utility class");
    }

    public static final String PDF_IMG_PAGOPA = "/templates/pdfReceipt/pagopa-logo.png";
    public static final String PDF_IMG_TRANSPARENT = "/templates/pdfReceipt/transparent-logo.png";
    public static final String PDF_TEXT_TITLE_PART1 = "Hai inviato un pagamento di €";
    public static final String PDF_TEXT_TITLE_PART2 = " tramite pagoPA";
    public static final String PDF_TEXT_NAME = "Intestatario";
    public static final String PDF_TEXT_ADDRESS = "Indirizzo e-mail";
    public static final String PDF_TEXT_PAYMENT_METHOD = "Metodo di pagamento";
    public static final String PDF_TEXT_TRANSACTION_MANAGER = "Gestore della transazione";
    public static final String PDF_TEXT_PAYMENT_DATE = "Data e ora del pagamento";
    public static final String PDF_TEXT_IUR = "ID della transazione (IUR)";
    public static final String PDF_TEXT_NOTICE_NUMBER = "AVVISO N.";
    public static final String PDF_TEXT_CREDITOR = "Ente Creditore Beneficiario";
    public static final String PDF_TEXT_DEBITOR = "Debitore";
    public static final String PDF_TEXT_PAYMENT_SUBJECT = "Oggetto del pagamento";
    public static final String PDF_TEXT_AMOUNT = "Importo (€)";
    public static final String PDF_TEXT_TRANSACTION_COST = "Costo transazione (€)";
    public static final String PDF_TEXT_TOTAL_AMOUNT = "Importo totale (€)";
    
}
