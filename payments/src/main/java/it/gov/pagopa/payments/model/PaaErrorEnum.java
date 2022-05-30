package it.gov.pagopa.payments.model;

import lombok.Getter;

@Getter
public enum PaaErrorEnum {
  PAA_ID_DOMINIO_ERRATO("PAA_ID_DOMINIO_ERRATO", "ID dominio errato", "L'idPA ricevuto non e' tra quelli configurati oppure non è associato alla stazione richiesta"),
  PAA_ID_INTERMEDIARIO_ERRATO("PAA_ID_INTERMEDIARIO_ERRATO", "IdBrokerPA errato",
      "L'IdBrokerPA ricevuto non e' tra quelli configurati"),
  PAA_STAZIONE_INT_ERRATA("PAA_STAZIONE_INT_ERRATA", "IdStazione errato",
      "L'IdStazione ricevuto non e' tra quelli configurati"),
  PAA_PAGAMENTO_ANNULLATO("PAA_PAGAMENTO_ANNULLATO", "pagamento annullato", "L'id del pagamento ricevuto e' annullato"),
  PAA_PAGAMENTO_SCONOSCIUTO("PAA_PAGAMENTO_SCONOSCIUTO", "pagamento sconosciuto",
      "L'id del pagamento ricevuto non esiste"),
  PAA_PAGAMENTO_DUPLICATO("PAA_PAGAMENTO_DUPLICATO", "pagamento duplicato",
          "L'id del pagamento ricevuto  e' duplicato"),
  PAA_RECEIPT_DUPLICATA("PAA_RECEIPT_DUPLICATA", "pagamento duplicato",
          "L'id del pagamento ricevuto  e' duplicato"),
  PAA_PAGAMENTO_SCADUTO("PAA_PAGAMENTO_SCADUTO", "pagamento scaduto",
          "L'id del pagamento ricevuto  e' scaduto"),
  PAA_SEMANTICA("PAA_SEMANTICA", "Errore Generico", "Errore Generico"),
  PAA_SINTASSI_XSD("PAA_SINTASSI_XSD", "Errore XSD", "Errore validazione XSD della request"),
  PAA_SYSTEM_ERROR("PAA_SYSTEM_ERROR", "Errore sistema", "Errore del sistema");

  private final String faultCode;
  private final String faultString;
  private final String description;

  PaaErrorEnum(String faultCode, String faultString, String description) {
    this.faultCode = faultCode;
    this.faultString = faultString;
    this.description = description;
  }
}
