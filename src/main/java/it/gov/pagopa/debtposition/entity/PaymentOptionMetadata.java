package it.gov.pagopa.debtposition.entity;

import com.fasterxml.jackson.annotation.JsonIdentityInfo;
import com.fasterxml.jackson.annotation.ObjectIdGenerators;
import java.io.Serializable;
import javax.persistence.CascadeType;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;
import javax.persistence.UniqueConstraint;
import javax.validation.constraints.NotNull;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Table(
    name = "payment_option_metadata",
    uniqueConstraints = {
      @UniqueConstraint(
          name = "UniquePaymentOptMetadata",
          columnNames = {"key", "payment_option_id"})
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

  @NotNull private String key;

  private String value;

  @ManyToOne(
      targetEntity = PaymentOption.class,
      fetch = FetchType.LAZY,
      optional = false,
      cascade = {CascadeType.PERSIST, CascadeType.MERGE})
  @JoinColumn(name = "payment_option_id")
  private PaymentOption paymentOption;
}
