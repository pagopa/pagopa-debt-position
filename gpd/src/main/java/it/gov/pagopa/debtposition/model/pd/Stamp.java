package it.gov.pagopa.debtposition.model.pd;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.validation.constraints.NotBlank;
import java.io.Serializable;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class Stamp implements Serializable {
  private static final long serialVersionUID = -5862140737726963810L;

  @NotBlank private String hashDocument;
  @NotBlank private String stampType;
  @NotBlank private String provincialResidence;
}
