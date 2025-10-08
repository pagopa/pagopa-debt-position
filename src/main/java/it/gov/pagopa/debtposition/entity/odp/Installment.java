package it.gov.pagopa.debtposition.entity.odp;

import com.fasterxml.jackson.annotation.JsonIdentityInfo;
import com.fasterxml.jackson.annotation.ObjectIdGenerators;
import it.gov.pagopa.debtposition.entity.odp.Transfer;
import it.gov.pagopa.debtposition.model.enumeration.InstallmentStatus;
import it.gov.pagopa.debtposition.model.enumeration.PaymentOptionStatus;
import jakarta.persistence.*;
import jakarta.validation.constraints.NotNull;
import lombok.*;

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
        name = "installment",
        uniqueConstraints = {
                @UniqueConstraint(name = "UniqueInstallmentIuv", columnNames = {"iuv", "organization_fiscal_code"}),
                @UniqueConstraint(name = "UniqueInstallmentNav", columnNames = {"nav", "organization_fiscal_code"})
        },
        indexes = {
                @Index(name = "idx_due_date", columnList = "debtor_fiscal_code"),
                @Index(name = "idx_payment_option_id_inst", columnList = "payment_option_id"),
                @Index(name = "idx_payment_position_id_inst", columnList = "payment_position_id"),
        })
@JsonIdentityInfo(
        generator = ObjectIdGenerators.IntSequenceGenerator.class,
        property = "@installmentId")
public class Installment {

    /**
     * generated serialVersionUID
     */
    private static final long serialVersionUID = -2800191377721368418L;

    @Id
    @GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "INSTALLMENT_SEQ")
    @SequenceGenerator(name = "INSTALLMENT_SEQ", sequenceName = "INSTALLMENT_SEQ", allocationSize = 1)
    private Long id;

    @NotNull
    private String nav;

    @NotNull
    private String iuv;

    @NotNull
    @Column(name = "organization_fiscal_code")
    private String organizationFiscalCode;

    @NotNull
    private long amount;

    private String description;

    @NotNull
    @Column(name = "due_date")
    private LocalDateTime dueDate;

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

    @NotNull
    @Column(name = "notification_fee")
    private long notificationFee;

    @Column(name = "psp_company")
    private String pspCompany;

    @Column(name = "receipt_id")
    private String receiptId;

    @Column(name = "flow_reporting_id")
    private String flowReportingId;

    @NotNull
    @Enumerated(EnumType.STRING)
    private InstallmentStatus status;

    @NotNull
    @Column(name = "last_updated_date")
    private LocalDateTime lastUpdatedDate;

    @Column(name = "last_updated_date_notification_fee")
    private LocalDateTime lastUpdatedDateNotificationFee;

    @Column(name = "payment_position_id")
    private Long paymentPositionId;

    @ManyToOne(
            targetEntity = PaymentOption.class,
            fetch = FetchType.LAZY,
            optional = false,
            cascade = {CascadeType.PERSIST, CascadeType.MERGE})
    @JoinColumn(name = "payment_option_id", nullable = false)
    private PaymentOption paymentOption;

    @Builder.Default
    @OneToMany(
            targetEntity = Transfer.class,
            fetch = FetchType.LAZY,
            mappedBy = "installment",
            cascade = CascadeType.ALL,
            orphanRemoval = true)
    private List<Transfer> transfer = new ArrayList<>();

    public void addTransfer(Transfer t) {
        transfer.add(t);
        t.setInstallment(this);
    }

    public void removeTransfer(Transfer t) {
        transfer.remove(t);
        t.setInstallment(null);
    }
}
