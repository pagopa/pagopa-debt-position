package it.gov.pagopa.debtposition.model;

import com.fasterxml.jackson.annotation.JsonInclude;

import lombok.Builder;
import lombok.Data;

@Data
@Builder
@JsonInclude(JsonInclude.Include.NON_NULL)
public class OdPErrorResponse {
  private Integer httpStatusCode;        // e.g. 404
  private String  httpStatusDescription; // e.g. "Not Found"
  private String  appErrorCode;          // e.g. "ODP-107"
  private Long    timestamp;             // e.g. 1724425035
  private String  dateTime;              // e.g. "2024-08-23T14:57:15.635528"
  private String  errorMessage;          // e.g. "PAA_* ... + <free text>"
}
