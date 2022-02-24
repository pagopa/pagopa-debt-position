package it.gov.pagopa.hubpa.payments.entity;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Table;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
@Entity
@Table(name = "incrementaliuvnumber")
public class IncrementalIuvNumber {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;
    @Column(name = "iddominiopa", nullable = false)
    private String idDominioPa;
    @Column(name = "lastusednumber", nullable = false)
    private Long lastUsedNumber;
    @Column(name = "anno", nullable = false)
    private Integer anno;

}
