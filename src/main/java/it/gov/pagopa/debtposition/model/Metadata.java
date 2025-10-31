package it.gov.pagopa.debtposition.model;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

import java.io.Serializable;

@Getter
@AllArgsConstructor
@NoArgsConstructor
public class Metadata implements Serializable {
    private String key;
    private String value;
}