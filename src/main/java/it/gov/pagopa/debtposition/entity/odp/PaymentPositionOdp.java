package it.gov.pagopa.debtposition.entity.odp;

import com.fasterxml.jackson.annotation.JsonIdentityInfo;
import com.fasterxml.jackson.annotation.ObjectIdGenerators;
import it.gov.pagopa.debtposition.model.enumeration.DebtPositionStatusV3;
import it.gov.pagopa.debtposition.model.enumeration.ServiceType;
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
        name = "payment_position",
        uniqueConstraints = {
                @UniqueConstraint(name = "UniquePaymentPos", columnNames = {"iupd", "organization_fiscal_code"})
        },
        indexes = {
                @Index(name = "idx_iupd", columnList = "iupd"),
                @Index(name = "idx_organization_fiscal_code", columnList = "organization_fiscal_code"),
                @Index(name = "idx_payment_date", columnList = "payment_date"),
                @Index(name = "idx_status_validity_date", columnList = "status, validity_date"),
                @Index(name = "idx_fiscal_code", columnList = "fiscal_code"),
        })
@JsonIdentityInfo(
        generator = ObjectIdGenerators.IntSequenceGenerator.class,
        property = "@paymentPositionId")
public class PaymentPositionOdp implements Serializable {

    @Id
    @GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "PAYMENT_POS_SEQ")
    @SequenceGenerator(name = "PAYMENT_POS_SEQ", sequenceName = "PAYMENT_POS_SEQ", allocationSize = 1)
    private Long id;

    @NotNull
    private String iupd;

    @NotNull
    @Column(name = "organization_fiscal_code")
    private String organizationFiscalCode;

    @Builder.Default
    @Column(name = "pull", columnDefinition = "boolean DEFAULT true")
    private Boolean pull = true;

    @Builder.Default
    @Column(name = "pay_stand_in", columnDefinition = "boolean DEFAULT true")
    private Boolean payStandIn = true;

    @Builder.Default
    @Enumerated(EnumType.STRING)
    @Column(name = "service_type")
    private ServiceType serviceType = ServiceType.GPD;

    // Payment Position properties
    @NotNull
    @Column(name = "company_name")
    private String companyName; // es. Comune di Roma

    @Column(name = "office_name")
    private String officeName; // es. Ufficio Tributi

    @NotNull
    @Column(name = "inserted_date")
    private LocalDateTime insertedDate;

    @Column(name = "publish_date")
    private LocalDateTime publishDate;

    @Column(name = "validity_date")
    private LocalDateTime validityDate;

    @NotNull
    @Column(name = "min_due_date")
    private LocalDateTime minDueDate;

    @NotNull
    @Column(name = "max_due_date")
    private LocalDateTime maxDueDate;

    @NotNull
    @Enumerated(EnumType.STRING)
    private DebtPositionStatusV3 status;

    @NotNull
    @Column(name = "last_updated_date")
    private LocalDateTime lastUpdatedDate;

    @Column(name = "payment_date")
    private LocalDateTime paymentDate;

    @Builder.Default
    @NotNull
    @Version
    @Column(columnDefinition = "integer DEFAULT 0")
    private Integer version = 0;

    @Builder.Default
    @OneToMany(
            targetEntity = PaymentOptionOdp.class,
            fetch = FetchType.LAZY,
            mappedBy = "paymentPositionOdp",
            cascade = CascadeType.ALL,
            orphanRemoval = true)
    private List<PaymentOptionOdp> paymentOptionOdp = new ArrayList<>();

    public void addPaymentOption(PaymentOptionOdp paymentOpt) {
        paymentOptionOdp.add(paymentOpt);
        paymentOpt.setPaymentPositionOdp(this);
    }
}