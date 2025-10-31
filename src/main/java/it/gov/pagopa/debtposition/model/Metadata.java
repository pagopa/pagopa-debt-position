package it.gov.pagopa.debtposition.model;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Size;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class Metadata implements Serializable {
    @NotBlank(message = "key is required")
    @Size(max = 140, message = "key must not exceed 140 characters")
    private String key;

    @NotBlank(message = "value is required")
    @Size(max = 140, message = "value must not exceed 140 characters")
    private String value;
}
