package it.gov.pagopa.debtposition.entity;

import com.fasterxml.jackson.annotation.JsonIdentityInfo;
import com.fasterxml.jackson.annotation.ObjectIdGenerators;
import it.gov.pagopa.debtposition.model.enumeration.DebtPositionStatusV3;
import it.gov.pagopa.debtposition.model.enumeration.OptionType;
import it.gov.pagopa.debtposition.model.enumeration.Type;
import jakarta.persistence.*;
import jakarta.validation.constraints.NotNull;
import lombok.*;

import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

@Builder(toBuilder = true)
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Entity
@Table(
        name = "payment_option",
//        uniqueConstraints = {
//                @UniqueConstraint(name = "UniquePaymentOpt", columnNames = {"payment_position_id", "organization_fiscal_code"}) TODO wrong contraint to change
//        },
        indexes = {
                @Index(name = "idx_debtor_fiscal_code", columnList = "debtor_fiscal_code"),
                // @Index(name = "idx_payment_option_metadata_gin", columnList = "payment_position_id"), TODO index metadata
                @Index(name = "idx_payment_position_id", columnList = "payment_position_id")
        })
@JsonIdentityInfo(
        generator = ObjectIdGenerators.IntSequenceGenerator.class,
        property = "@paymentOptionId")
public class PaymentOption implements Serializable {

    @Id
    @GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "PAYMENT_OPT_SEQ")
    @SequenceGenerator(name = "PAYMENT_OPT_SEQ", sequenceName = "PAYMENT_OPT_SEQ", allocationSize = 1)
    private Long id;

    @NotNull
    @Column(name = "organization_fiscal_code")
    private String organizationFiscalCode;

    private String description;

    @Column(name = "validity_date")
    private LocalDateTime validityDate;

    @Column(name = "retention_date")
    private LocalDateTime retentionDate;

    @NotNull
    @Column(name = "inserted_date")
    private LocalDateTime insertedDate;

    @NotNull
    @Enumerated(EnumType.STRING)
    @Column(name = "option_type")
    private OptionType optionType;

    @Column(name = "payment_position_status")
    @Enumerated(EnumType.STRING)
    private DebtPositionStatusV3 paymentPositionStatus;

    @NotNull
    @Column(name = "switch_to_expired")
    private Boolean switchToExpired;


    // Debtor properties
    @NotNull
    @Enumerated(EnumType.STRING)
    @Column(name = "debtor_type")
    @ToString.Exclude
    private Type debtorType;

    @NotNull
    @Column(name = "debtor_fiscal_code")
    @ToString.Exclude
    private String debtorFiscalCode;

    @NotNull
    @Column(name = "debtor_full_name")
    @ToString.Exclude
    private String debtorFullName;

    @Column(name = "debtor_street_name")
    @ToString.Exclude
    private String debtorStreetName;

    @Column(name = "debtor_civic_number")
    @ToString.Exclude
    private String debtorCivicNumber;

    @Column(name = "debtor_postal_code")
    @ToString.Exclude
    private String debtorPostalCode;

    @Column(name = "debtor_city")
    @ToString.Exclude
    private String debtorCity;

    @Column(name = "debtor_province")
    @ToString.Exclude
    private String debtorProvince;

    @Column(name = "debtor_region")
    @ToString.Exclude
    private String debtorRegion;

    @Column(name = "debtor_country")
    @ToString.Exclude
    private String debtorCountry;

    @Column(name = "debtor_email")
    @ToString.Exclude
    private String debtorEmail;

    @Column(name = "debtor_phone")
    @ToString.Exclude
    private String debtorPhone;


    @ManyToOne(
            targetEntity = PaymentPosition.class,
            fetch = FetchType.LAZY,
            optional = false,
            cascade = {CascadeType.PERSIST, CascadeType.MERGE})
    @JoinColumn(name = "payment_position_id", nullable = false)
    private PaymentPosition paymentPosition;

    @Builder.Default
    @OneToMany(
            targetEntity = Installment.class,
            fetch = FetchType.LAZY,
            mappedBy = "paymentOption",
            cascade = CascadeType.ALL,
            orphanRemoval = true)
    private List<Installment> installment = new ArrayList<>();

    public void addInstallment(Installment i) {
        installment.add(i);
        i.setPaymentOption(this);
    }

    public void removeInstallment(Installment i) {
        installment.remove(i);
        i.setPaymentOption(null);
    }
}