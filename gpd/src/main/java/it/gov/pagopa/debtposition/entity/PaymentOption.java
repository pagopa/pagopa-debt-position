package it.gov.pagopa.debtposition.entity;

import it.gov.pagopa.debtposition.model.enumeration.PaymentOptionStatus;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import javax.persistence.*;
import javax.validation.constraints.NotNull;
import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

/**
 * @author aacitelli
 * <p>JPA Entity
 */
@Builder(toBuilder = true)
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Entity
@Table(
        name = "payment_option",
        uniqueConstraints = {
                @UniqueConstraint(
                        name = "UniquePaymentOpt",
                        columnNames = {"iuv", "organization_fiscal_code"})
        },
        indexes = @Index(name = "payment_option_payment_position_id_idx", columnList = "payment_position_id"))
public class PaymentOption implements Serializable {

    /**
     * generated serialVersionUID
     */
    private static final long serialVersionUID = -2800191377721368418L;

    @Id
    @GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "PAYMENT_OPT_SEQ")
    @SequenceGenerator(name = "PAYMENT_OPT_SEQ", sequenceName = "PAYMENT_OPT_SEQ", allocationSize = 1)
    private Long id;

    @NotNull
    private String iuv;

    @NotNull
    @Column(name = "organization_fiscal_code")
    private String organizationFiscalCode;

    @NotNull
    private long amount;
    private String description;

    @NotNull
    @Column(name = "is_partial_payment")
    private Boolean isPartialPayment;

    @NotNull
    @Column(name = "due_date")
    private LocalDateTime dueDate;

    @Column(name = "retention_date")
    private LocalDateTime retentionDate;

    @Column(name = "payment_date")
    private LocalDateTime paymentDate;

    @Column(name = "reporting_date")
    private LocalDateTime reportingDate;

    @NotNull
    @Column(name = "inserted_date")
    private LocalDateTime insertedDate;

    @Column(name = "payment_method")
    private String paymentMethod;

    private long fee;

    @Column(name = "psp_company")
    private String pspCompany;

    @Column(name = "receipt_id")
    private String idReceipt;

    @Column(name = "flow_reporting_id")
    private String idFlowReporting;

    @NotNull
    @Enumerated(EnumType.STRING)
    private PaymentOptionStatus status;

    @NotNull
    @Column(name = "last_updated_date")
    private LocalDateTime lastUpdatedDate;

    @ManyToOne(
            targetEntity = PaymentPosition.class,
            fetch = FetchType.LAZY,
            optional = false,
            cascade = {CascadeType.PERSIST, CascadeType.MERGE})
    @JoinColumn(name = "payment_position_id")
    private PaymentPosition paymentPosition;

    @Builder.Default
    @OneToMany(
            targetEntity = Transfer.class,
            fetch = FetchType.LAZY,
            mappedBy = "paymentOption",
            cascade = CascadeType.ALL,
            orphanRemoval = true)
    private List<Transfer> transfer = new ArrayList<>();

    public void addTransfer(Transfer t) {
        transfer.add(t);
        t.setPaymentOption(this);
    }

    public void removeTransfer(Transfer t) {
        transfer.remove(t);
        t.setPaymentOption(null);
    }
}
