package it.gov.pagopa.debtposition.entity;

import com.fasterxml.jackson.annotation.JsonIdentityInfo;
import com.fasterxml.jackson.annotation.ObjectIdGenerators;
import it.gov.pagopa.debtposition.model.enumeration.DebtPositionStatus;
import it.gov.pagopa.debtposition.model.enumeration.ServiceType;
import it.gov.pagopa.debtposition.model.enumeration.Type;
import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import jakarta.persistence.*;
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
    name = "payment_position",
    uniqueConstraints = {
      @UniqueConstraint(
          name = "UniquePaymentPos",
          columnNames = {"iupd", "organization_fiscal_code"})
    })
@JsonIdentityInfo(
    generator = ObjectIdGenerators.IntSequenceGenerator.class,
    property = "@paymentPositionId")
public class PaymentPosition implements Serializable {

  /** generated serialVersionUID */
  private static final long serialVersionUID = -8637183968286214359L;

  @Id
  @GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "PAYMENT_POS_SEQ")
  @SequenceGenerator(name = "PAYMENT_POS_SEQ", sequenceName = "PAYMENT_POS_SEQ", allocationSize = 1)
  private Long id;

  @NotNull private String iupd;

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
  @ToString.Exclude private String email;
  @ToString.Exclude private String phone;

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
  @NotNull
  @Version
  @Column(columnDefinition = "integer DEFAULT 0")
  private Integer version = 0;

  @Builder.Default
  @OneToMany(
      targetEntity = PaymentOption.class,
      fetch = FetchType.LAZY,
      mappedBy = "paymentPosition",
      cascade = CascadeType.ALL,
      orphanRemoval = true)
  private List<PaymentOption> paymentOption = new ArrayList<>();

  public void addPaymentOption(PaymentOption paymentOpt) {
    paymentOption.add(paymentOpt);
    paymentOpt.setPaymentPosition(this);
  }
}
