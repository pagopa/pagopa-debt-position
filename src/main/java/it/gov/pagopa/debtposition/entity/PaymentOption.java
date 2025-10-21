package it.gov.pagopa.debtposition.entity;

import com.fasterxml.jackson.annotation.JsonIdentityInfo;
import com.fasterxml.jackson.annotation.ObjectIdGenerators;
import it.gov.pagopa.debtposition.model.enumeration.PaymentOptionStatus;
import it.gov.pagopa.debtposition.model.enumeration.Type;
import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import jakarta.persistence.CascadeType;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import jakarta.persistence.FetchType;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.OneToMany;
import jakarta.persistence.SequenceGenerator;
import jakarta.persistence.Table;
import jakarta.persistence.Transient;
import jakarta.persistence.UniqueConstraint;
import jakarta.validation.constraints.NotNull;
import lombok.*;

/**
 * @author aacitelli
 *     <p>JPA Entity
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
          columnNames = {"iuv", "organization_fiscal_code"}),
      @UniqueConstraint(
          name = "UniquePaymentOptNav",
          columnNames = {"nav", "organization_fiscal_code"}),
    })
@JsonIdentityInfo(
	    generator = ObjectIdGenerators.PropertyGenerator.class,
	    property = "id")
public class PaymentOption implements Serializable {

  /** generated serialVersionUID */
  private static final long serialVersionUID = -2800191377721368418L;

  @Id
  @GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "PAYMENT_OPT_SEQ")
  @SequenceGenerator(name = "PAYMENT_OPT_SEQ", sequenceName = "PAYMENT_OPT_SEQ", allocationSize = 1)
  private Long id;

  @NotNull private String nav;

  @NotNull private String iuv;

  @NotNull
  @Column(name = "organization_fiscal_code")
  private String organizationFiscalCode;
  
  // payment_plan_id (null for single, '<uuid>' for installment plans)
  @Column(name = "payment_plan_id", length = 50)
  private String paymentPlanId;

  @NotNull private long amount;
  private String description;

  @NotNull
  @Column(name = "is_partial_payment")
  private Boolean isPartialPayment;
  
  @Column(name = "validity_date")
  private LocalDateTime validityDate;

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

  @Column(name = "notification_fee")
  private long notificationFee;

  @Column(name = "psp_code")
  private String pspCode;

  @Column(name = "psp_tax_code")
  private String pspTaxCode;

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

  @Column(name = "last_updated_date_notification_fee")
  private LocalDateTime lastUpdatedDateNotificationFee;

  // Debtor properties
  @NotNull
  @Enumerated(EnumType.STRING)
  @Column(name = "type")
  @ToString.Exclude
  private Type debtorType;

  @NotNull
  @Column(name = "fiscal_code")
  @ToString.Exclude
  private String fiscalCode;

  @NotNull
  @Column(name = "full_name")
  @ToString.Exclude
  private String fullName;

  @Column(name = "street_name")
  @ToString.Exclude
  private String streetName;

  @Column(name = "civic_number")
  @ToString.Exclude
  private String civicNumber;

  @Column(name = "postal_code")
  @ToString.Exclude
  private String postalCode;

  @ToString.Exclude private String city;
  @ToString.Exclude private String province;
  @ToString.Exclude private String region;
  @ToString.Exclude private String country;
  @ToString.Exclude private String email;
  @ToString.Exclude private String phone;

  @Column(name = "send_sync")
  @Builder.Default private Boolean sendSync = false;
  
  @Builder.Default
  @Column(name = "switch_to_expired", columnDefinition = "boolean DEFAULT false")
  private Boolean switchToExpired = false;

  // flag that identifies if the payment option has a payment in progress (false = no payment in
  // progress)
  @Builder.Default @Transient private boolean paymentInProgress = false;

  @ManyToOne(
      targetEntity = PaymentPosition.class,
      fetch = FetchType.LAZY,
      optional = false,
      cascade = {CascadeType.PERSIST, CascadeType.MERGE})
  @JoinColumn(name = "payment_position_id", nullable = false)
  private PaymentPosition paymentPosition;

  @Builder.Default
  @OneToMany(
      targetEntity = Transfer.class,
      fetch = FetchType.LAZY,
      mappedBy = "paymentOption",
      cascade = CascadeType.ALL,
      orphanRemoval = true)
  private List<Transfer> transfer = new ArrayList<>();

  @Builder.Default
  @OneToMany(
      targetEntity = PaymentOptionMetadata.class,
      fetch = FetchType.LAZY,
      mappedBy = "paymentOption",
      cascade = CascadeType.ALL,
      orphanRemoval = true)
  private List<PaymentOptionMetadata> paymentOptionMetadata = new ArrayList<>();

  public void addTransfer(Transfer t) {
    transfer.add(t);
    t.setPaymentOption(this);
  }

  public void removeTransfer(Transfer t) {
    transfer.remove(t);
    t.setPaymentOption(null);
  }

  public void addPaymentOptionMetadata(PaymentOptionMetadata paymentOptMetadata) {
    paymentOptionMetadata.add(paymentOptMetadata);
    paymentOptMetadata.setPaymentOption(this);
  }

  public void removePaymentOptionMetadata(PaymentOptionMetadata paymentOptMetadata) {
    paymentOptionMetadata.remove(paymentOptMetadata);
    paymentOptMetadata.setPaymentOption(null);
  }
}
