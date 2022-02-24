package it.gov.pagopa.payments.entity;

import java.math.BigDecimal;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

import com.fasterxml.jackson.annotation.JsonIgnore;

import org.hibernate.annotations.Check;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
@Entity
@Table(name = "transfers")
/**
 * At least one of postal iban and bank iban 
 */
@Check(constraints = "postal_iban IS NOT NULL OR iban IS NOT NULL ")
public class Transfers {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(name = "partial_amount", nullable = false)
    private BigDecimal partialAmount;
    @Column(name = "iban", nullable = true)
    private String iban;
    @Column(name = "organization_fiscal_code", nullable = false)
    private String organizationFiscalCode;
    @Column(name = "reason", nullable = false)
    private String reason;
    @Column(name = "taxonomy", nullable = false)
    private String taxonomy;
    @Column(name = "postal_iban", nullable = true)
    private String postalIban;
    @Column(name = "postal_iban_holder", nullable = true)
    private String postalIbanHolder;
    @Column(name = "postal_auth_code", nullable = true)
    private String postalAuthCode;

    @ManyToOne(targetEntity = PaymentOptions.class, fetch = FetchType.LAZY, cascade = {CascadeType.PERSIST})
    @JoinColumn(name = "payment_option_id")
    @JsonIgnore
    private PaymentOptions paymentOptions;

}
