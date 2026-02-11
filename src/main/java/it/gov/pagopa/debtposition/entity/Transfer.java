package it.gov.pagopa.debtposition.entity;

import com.fasterxml.jackson.annotation.JsonIdentityInfo;
import com.fasterxml.jackson.annotation.ObjectIdGenerators;
import it.gov.pagopa.debtposition.model.enumeration.TransferStatus;
import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

import org.hibernate.annotations.BatchSize;

import jakarta.persistence.CascadeType;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import jakarta.persistence.FetchType;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.Index;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.OneToMany;
import jakarta.persistence.SequenceGenerator;
import jakarta.persistence.Table;
import jakarta.persistence.UniqueConstraint;
import jakarta.validation.constraints.NotNull;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

/**
 * @author aacitelli
 *     <p>JPA Entity
 * @param <transferMetadata>
 */
@Builder(toBuilder = true)
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Entity
@Table(
    name = "transfer",
    uniqueConstraints = {
      @UniqueConstraint(
          name = "unique_transfer",
          columnNames = {"iuv", "organization_fiscal_code", "transfer_id", "payment_option_id"})
    },
    indexes = @Index(name = "transfer_payment_option_id_idx", columnList = "payment_option_id"))
@JsonIdentityInfo(
    generator = ObjectIdGenerators.IntSequenceGenerator.class,
    property = "@transferId")
public class Transfer implements Serializable {

  /** generated serialVersionUID */
  private static final long serialVersionUID = -886970813082991109L;

  @Id
  @GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "TRANSFER_SEQ")
  @SequenceGenerator(name = "TRANSFER_SEQ", sequenceName = "TRANSFER_SEQ", allocationSize = 1)
  private Long id;

  @NotNull
  @Column(name = "organization_fiscal_code")
  private String organizationFiscalCode;

  @NotNull
  @Column(name = "transfer_id")
  private String idTransfer;

  @NotNull private String iuv;
  @NotNull private long amount;

  @NotNull
  @Column(name = "remittance_information")
  private String remittanceInformation; // causale

  @NotNull private String category; // taxonomy

  private String iban;

  @Column(name = "postal_iban")
  private String postalIban;

  @Column(name = "hash_document")
  private String hashDocument;

  @Column(name = "stamp_type")
  private String stampType;

  @Column(name = "provincial_residence")
  private String provincialResidence;

  @Column(name = "company_name")
  private String companyName;

  @NotNull
  @Column(name = "inserted_date")
  private LocalDateTime insertedDate;

  @NotNull
  @Enumerated(EnumType.STRING)
  private TransferStatus status;

  @NotNull
  @Column(name = "last_updated_date")
  private LocalDateTime lastUpdatedDate;

  @ManyToOne(
      targetEntity = PaymentOption.class,
      fetch = FetchType.LAZY,
      optional = false,
      cascade = {CascadeType.PERSIST, CascadeType.MERGE})
  @JoinColumn(name = "payment_option_id")
  private PaymentOption paymentOption;

  @Builder.Default
  @BatchSize(size = 25)
  @OneToMany(
      targetEntity = TransferMetadata.class,
      fetch = FetchType.LAZY,
      mappedBy = "transfer",
      cascade = CascadeType.ALL,
      orphanRemoval = true)
  private List<TransferMetadata> transferMetadata = new ArrayList<>();

  public void addTransferMetadata(TransferMetadata metadata) {
    transferMetadata.add(metadata);
    metadata.setTransfer(this);
  }

  public void removeTransferMetadata(TransferMetadata metadata) {
    transferMetadata.remove(metadata);
    metadata.setTransfer(null);
  }
}
