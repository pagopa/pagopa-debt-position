package it.gov.pagopa.debtposition.entity;

import it.gov.pagopa.debtposition.model.enumeration.DebtPositionStatus;
import it.gov.pagopa.debtposition.model.enumeration.Type;
import lombok.*;

import javax.persistence.*;
import javax.validation.constraints.NotNull;

import com.fasterxml.jackson.annotation.JsonIdentityInfo;
import com.fasterxml.jackson.annotation.ObjectIdGenerators;
import org.apache.commons.lang3.SerializationUtils;

import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

/**
 * @author aacitelli
 * <p>
 * JPA Entity
 */

@Builder(toBuilder = true)
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Entity
@Table(name = "payment_position", uniqueConstraints = {
        @UniqueConstraint(name = "UniquePaymentPos", columnNames = {"iupd", "organization_fiscal_code"})
        },
        indexes = {
            @Index(name = "payment_position_status_validity_date_idx", columnList = "status, validity_date"),
            @Index(name = "idx_fiscal_code", columnList = "fiscal_code")
        }
)
@JsonIdentityInfo(generator=ObjectIdGenerators.IntSequenceGenerator.class, property="@paymentPositionId")
public class PaymentPosition implements Serializable {


    /**
     * generated serialVersionUID
     */
    private static final long serialVersionUID = -8637183968286214359L;


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

    // Debtor properties
    @NotNull
    @Enumerated(EnumType.STRING)
    private Type type;
    @NotNull
    @Column(name = "fiscal_code")
    private String fiscalCode;
    @NotNull
    @Column(name = "full_name")
    @ToString.Exclude
    private String fullName;
    @Column(name = "street_name")
    private String streetName;
    @Column(name = "civic_number")
    private String civicNumber;
    @Column(name = "postal_code")
    private String postalCode;
    private String city;
    private String province;
    private String region;
    private String country;
    @ToString.Exclude
    private String email;
    @ToString.Exclude
    private String phone;

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
    private DebtPositionStatus status;
    @NotNull
    @Column(name = "last_updated_date")
    private LocalDateTime lastUpdatedDate;
    @Column(name = "payment_date")
    private LocalDateTime paymentDate;
    @Builder.Default
    @Column(name = "switch_to_expired", columnDefinition = "boolean DEFAULT false")
    private Boolean switchToExpired = false;

    @Builder.Default
    @NotNull
    @Version
    @Column(columnDefinition = "integer DEFAULT 0")
    private Integer version = 0;

    @Builder.Default
    @OneToMany(targetEntity = PaymentOption.class, fetch = FetchType.LAZY, mappedBy = "paymentPosition", cascade = CascadeType.ALL, orphanRemoval = true)
    private List<PaymentOption> paymentOption = new ArrayList<>();

    public void addPaymentOption(PaymentOption paymentOpt) {
        paymentOption.add(paymentOpt);
        paymentOpt.setPaymentPosition(this);
    }

    public void removePaymentOption(PaymentOption paymentOpt) {
        paymentOption.remove(paymentOpt);
        paymentOpt.setPaymentPosition(null);
    }

    public PaymentPosition deepClone() {
        PaymentPosition clone = SerializationUtils.clone(this);
        List<PaymentOption> clonedPaymentOptions = new ArrayList<>();
        paymentOption.forEach(po -> clonedPaymentOptions.add(SerializationUtils.clone(po)));
        clone.setPaymentOption(clonedPaymentOptions);

        return clone;
    }
}
