package it.gov.pagopa.debtposition.entity.apd;

import com.fasterxml.jackson.annotation.JsonIdentityInfo;
import com.fasterxml.jackson.annotation.ObjectIdGenerators;
import java.io.Serializable;
import jakarta.persistence.CascadeType;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.FetchType;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.SequenceGenerator;
import jakarta.persistence.Table;
import jakarta.persistence.UniqueConstraint;
import jakarta.validation.constraints.NotNull;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Table(
    name = "payment_option_metadata",
    uniqueConstraints = {
      @UniqueConstraint(
          name = "UniquePaymentOptMetadata",
          columnNames = {"\"key\"", "payment_option_id"})
    })
@Data
@NoArgsConstructor
@AllArgsConstructor
@Entity
@Builder
@JsonIdentityInfo(
    generator = ObjectIdGenerators.IntSequenceGenerator.class,
    property = "@paymentOptionMetadataId")
public class PaymentOptionMetadata implements Serializable {

  /** generated serialVersionUID */
  private static final long serialVersionUID = -9014105148787448923L;

  @Id
  @GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "PAYMENT_OPT_METADATA_SEQ")
  @SequenceGenerator(
      name = "PAYMENT_OPT_METADATA_SEQ",
      sequenceName = "PAYMENT_OPT_METADATA_SEQ",
      allocationSize = 1)
  private Long id;

  @NotNull
  @Column(name = "\"key\"", nullable = false)
  private String key;

  @Column(name = "\"value\"")
  private String value;

  @ManyToOne(
      targetEntity = PaymentOption.class,
      fetch = FetchType.LAZY,
      optional = false,
      cascade = {CascadeType.PERSIST, CascadeType.MERGE})
  @JoinColumn(name = "payment_option_id")
  private PaymentOption paymentOption;
}
