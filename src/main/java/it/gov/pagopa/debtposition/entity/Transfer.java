package it.gov.pagopa.debtposition.entity;

import com.fasterxml.jackson.annotation.JsonIdentityInfo;
import com.fasterxml.jackson.annotation.ObjectIdGenerators;
import it.gov.pagopa.debtposition.model.enumeration.TransferStatus;
import jakarta.persistence.*;
import jakarta.validation.constraints.NotNull;
import lombok.*;

import java.io.Serializable;
import java.time.LocalDateTime;

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
                        name = "UniqueTransfer",
                        columnNames = {"iuv", "organization_fiscal_code", "transfer_id", "installment_id"})
        },
        indexes = {
                @Index(name = "idx_installment_id", columnList = "installment_id"),
                @Index(name = "idx_transfer_metadata_gin", columnList = "metadata"),
        })
@JsonIdentityInfo(
        generator = ObjectIdGenerators.IntSequenceGenerator.class,
        property = "@transferId")
public class Transfer implements Serializable {

    @Id
    @GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "TRANSFER_SEQ")
    @SequenceGenerator(name = "TRANSFER_SEQ", sequenceName = "TRANSFER_SEQ", allocationSize = 1)
    private Long id;

    @NotNull
    @Column(name = "organization_fiscal_code")
    private String organizationFiscalCode;

    @NotNull
    @Column(name = "transfer_id")
    private String transferId;

    @NotNull
    private String iuv;

    @NotNull
    private long amount;

    @NotNull
    @Column(name = "remittance_information")
    private String remittanceInformation; // causale

    @NotNull
    private String category; // taxonomy

    private String iban;

    @Column(name = "postal_iban")
    private String postalIban;

    @Column(name = "hash_document")
    private String hashDocument;

    @Column(name = "stamp_type")
    private String stampType;

    @Column(name = "provincial_residence")
    private String provincialResidence;

    @NotNull
    @Column(name = "inserted_date")
    private LocalDateTime insertedDate;

    @NotNull
    @Enumerated(EnumType.STRING)
    private TransferStatus status;

    @NotNull
    @Column(name = "last_updated_date")
    private LocalDateTime lastUpdatedDate;

    @Column(columnDefinition = "jsonb")
    private String metadata;

    @ManyToOne(
            targetEntity = Installment.class,
            fetch = FetchType.LAZY,
            optional = false,
            cascade = {CascadeType.PERSIST, CascadeType.MERGE})
    @JoinColumn(name = "installment_id")
    private Installment installment;
}