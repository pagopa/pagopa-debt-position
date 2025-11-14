package it.gov.pagopa.debtposition.exception;

import org.springframework.http.HttpStatus;

public enum OdpProfile {
  SYNTAX("ODP-101", HttpStatus.BAD_REQUEST, "PAA_SINTASSI"),
  SEMANTIC("ODP-102", HttpStatus.UNPROCESSABLE_ENTITY, "PAA_SEMANTICA"),
  SYSTEM("ODP-103", HttpStatus.INTERNAL_SERVER_ERROR, "PAA_SYSTEM_ERROR"),
  WRONG_DOMAIN("ODP-104", HttpStatus.BAD_REQUEST, "PAA_ID_DOMINIO_ERRATO"),
  WRONG_BROKER("ODP-105", HttpStatus.BAD_REQUEST, "PAA_ID_INTERMEDIARIO_ERRATO"),
  WRONG_STATION("ODP-106", HttpStatus.BAD_REQUEST, "PAA_STAZIONE_INT_ERRATA"),
  UNKNOWN_PAYMENT("ODP-107", HttpStatus.NOT_FOUND, "PAA_PAGAMENTO_SCONOSCIUTO"),
  DUPLICATED("ODP-108", HttpStatus.CONFLICT, "PAA_PAGAMENTO_DUPLICATO"),
  IN_PROGRESS("ODP-109", HttpStatus.CONFLICT, "PAA_PAGAMENTO_IN_CORSO"),
  EXPIRED("ODP-110", HttpStatus.UNPROCESSABLE_ENTITY, "PAA_PAGAMENTO_SCADUTO"),
  CANCELED("ODP-111", HttpStatus.UNPROCESSABLE_ENTITY, "PAA_PAGAMENTO_ANNULLATO");

  public final String code;
  public final HttpStatus http;
  public final String paa;

  OdpProfile(String code, HttpStatus http, String paa) {
    this.code = code;
    this.http = http;
    this.paa = paa;
  }
}
