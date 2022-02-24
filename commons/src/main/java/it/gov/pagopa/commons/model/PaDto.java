package it.gov.pagopa.commons.model;

import javax.validation.constraints.Email;
import javax.validation.constraints.NotBlank;

import it.gov.pagopa.commons.annotation.validation.IvaCode;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@NoArgsConstructor
@AllArgsConstructor
@Getter
@Setter
public class PaDto {

  private Long id;
  private String codAmm;
  @NotBlank
  private String desAmm;
  private String tipologiaIstat;
  @IvaCode
  @NotBlank
  private String codiceFiscale;
  private String codiceInterbancario;
  private String indirizzo;
  private String comune;
  private String cap;
  private String provincia;
  @Email
  private String emailCertificata;
  private String sitoIstituzionale;
  private Boolean allowed;
}
