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
    name = "transfer_metadata",
    uniqueConstraints = {
      @UniqueConstraint(
          name = "UniqueTransferMetadata",
          columnNames = {"\"key\"", "transfer_id"})
    })
@Data
@NoArgsConstructor
@AllArgsConstructor
@Entity
@Builder
@JsonIdentityInfo(
    generator = ObjectIdGenerators.IntSequenceGenerator.class,
    property = "@transferMetadataId")
public class TransferMetadata implements Serializable {

  /** generated serialVersionUID */
  private static final long serialVersionUID = -385216542341056723L;

  @Id
  @GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "TRANSFER_METADATA_SEQ")
  @SequenceGenerator(
      name = "TRANSFER_METADATA_SEQ",
      sequenceName = "TRANSFER_METADATA_SEQ",
      allocationSize = 1)
  private Long id;

  @NotNull
  @Column(name = "\"key\"", nullable = false)
  private String key;

  @Column(name = "\"value\"")
  private String value;

  @ManyToOne(
      targetEntity = Transfer.class,
      fetch = FetchType.LAZY,
      optional = false,
      cascade = {CascadeType.PERSIST, CascadeType.MERGE})
  @JoinColumn(name = "transfer_id")
  private Transfer transfer;
}
