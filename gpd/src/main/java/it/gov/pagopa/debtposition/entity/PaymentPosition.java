package it.gov.pagopa.debtposition.entity;

import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;
import javax.persistence.UniqueConstraint;
import javax.validation.constraints.NotNull;

import it.gov.pagopa.debtposition.model.enumeration.DebtPositionStatus;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

/**
 * @author aacitelli
 * 
 * JPA Entity
 *
 */


@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Entity
@Table (name = "payment_position", uniqueConstraints={
        @UniqueConstraint(name = "UniquePaymentPos", columnNames = {"iupd", "organization_fiscal_code"})
})
public class PaymentPosition implements Serializable {


    /**
     * generated serialVersionUID
     */
    private static final long serialVersionUID = -8637183968286214359L;


    @Id
    @GeneratedValue (strategy = GenerationType.SEQUENCE, generator="PAYMENT_POS_SEQ")
    @SequenceGenerator(name="PAYMENT_POS_SEQ", sequenceName="PAYMENT_POS_SEQ", allocationSize=1)
    private long id;

    @NotNull
    private String iupd;
    @NotNull
    @Column (name="organization_fiscal_code")
    private String organizationFiscalCode;

    @NotNull
    @Column (name="company_name")
    private String companyName; // es. Comune di Roma
    @Column (name="office_name")
    private String officeName; // es. Ufficio Tributi
    @NotNull
    @Column (name="inserted_date")
    private LocalDateTime insertedDate;
    @Column (name="publish_date")
    private LocalDateTime publishDate;
    @Column (name="validity_date")
    private LocalDateTime validityDate;
    @NotNull
    @Enumerated(EnumType.STRING)
    private DebtPositionStatus status;
    @NotNull
    @Column (name="last_updated_date")
    private LocalDateTime lastUpdatedDate;


    @ManyToOne(targetEntity = Debtor.class, fetch = FetchType.LAZY, optional = false, cascade = {CascadeType.PERSIST})
    @JoinColumn(name = "debtor_id")
    private Debtor debtor;


    @OneToMany(targetEntity = PaymentOption.class, fetch = FetchType.LAZY, mappedBy = "paymentPosition", cascade = CascadeType.ALL, orphanRemoval = true)
    private List<PaymentOption> paymentOption = new ArrayList<>();

    public void addPaymentOptions(PaymentOption paymentOpt) {
        paymentOption.add(paymentOpt);
        paymentOpt.setPaymentPosition(this);
    }

    public void removePaymentOptions(PaymentOption paymentOpt) {
        paymentOption.remove(paymentOpt);
        paymentOpt.setPaymentPosition(null);
    }
}
