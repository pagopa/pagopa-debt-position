package it.gov.pagopa.debtposition.entity.odp;

import com.fasterxml.jackson.annotation.JsonIdentityInfo;
import com.fasterxml.jackson.annotation.ObjectIdGenerators;
import it.gov.pagopa.debtposition.model.enumeration.InstallmentStatus;
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
public class Installment implements Serializable {

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

    @Column(name = "psp_code")
    private String pspCode;

    @Column(name = "psp_tax_code")
    private String pspTaxCode;

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

    @NotNull
    @Column(name = "send_sync")
    private Boolean sendSync;

    @ManyToOne(
            targetEntity = PaymentPositionOdp.class,
            fetch = FetchType.LAZY,
            optional = false,
            cascade = {CascadeType.PERSIST, CascadeType.MERGE})
    @JoinColumn(name = "payment_position_id", nullable = false)
    private PaymentPositionOdp paymentPositionOdp;

    @ManyToOne(
            targetEntity = PaymentOptionOdp.class,
            fetch = FetchType.LAZY,
            optional = false,
            cascade = {CascadeType.PERSIST, CascadeType.MERGE})
    @JoinColumn(name = "payment_option_id", nullable = false)
    private PaymentOptionOdp paymentOptionOdp;

    @Builder.Default
    @OneToMany(
            targetEntity = TransferOdp.class,
            fetch = FetchType.LAZY,
            mappedBy = "installment",
            cascade = CascadeType.ALL,
            orphanRemoval = true)
    private List<TransferOdp> transferOdp = new ArrayList<>();

    public void addTransfer(TransferOdp t) {
        transferOdp.add(t);
        t.setInstallment(this);
    }

    public void removeTransfer(TransferOdp t) {
        transferOdp.remove(t);
        t.setInstallment(null);
    }
}
