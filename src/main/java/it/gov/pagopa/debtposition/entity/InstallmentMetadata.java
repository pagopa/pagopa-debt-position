package it.gov.pagopa.debtposition.entity;

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
    name = "installment_metadata",
    uniqueConstraints = {
      @UniqueConstraint(
          name = "uniqueinstallmentmetadata",
          columnNames = {"\"key\"", "installment_id"})
    })
@Data
@NoArgsConstructor
@AllArgsConstructor
@Entity
@Builder
@JsonIdentityInfo(
	    generator = ObjectIdGenerators.PropertyGenerator.class,
	    property = "id")
public class InstallmentMetadata implements Serializable {

  /** generated serialVersionUID */
  private static final long serialVersionUID = -9014105148787448923L;

  @Id
  @GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "INSTALLMENT_METADATA_SEQ")
  @SequenceGenerator(
      name = "INSTALLMENT_METADATA_SEQ",
      sequenceName = "INSTALLMENT_METADATA_SEQ",
      allocationSize = 1)
  private Long id;

  @NotNull
  @Column(name = "\"key\"", nullable = false)
  private String key;

  @Column(name = "\"value\"")
  private String value;

  @ManyToOne(
      targetEntity = Installment.class,
      fetch = FetchType.LAZY,
      optional = false,
      cascade = {CascadeType.PERSIST, CascadeType.MERGE})
  @JoinColumn(name = "installment_id")
  private Installment installment;
}
