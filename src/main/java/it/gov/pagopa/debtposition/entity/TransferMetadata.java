package it.gov.pagopa.debtposition.entity;

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
        name = "transfer_metadata",
        uniqueConstraints = {
                @UniqueConstraint(
                        name = "UniqueTransferMetadata",
                        columnNames = {"key", "transfer_id"})
        })
@Data
@NoArgsConstructor
@AllArgsConstructor
@Entity
@Builder
public class TransferMetadata implements Serializable {

	/**
	 * generated serialVersionUID
	 */
	private static final long serialVersionUID = -385216542341056723L;

	@Id
    @GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "TRANSFER_METADATA_SEQ")
    @SequenceGenerator(name = "TRANSFER_METADATA_SEQ", sequenceName = "TRANSFER_METADATA_SEQ", allocationSize = 1)
    private Long id;

    @NotNull
    private String key;

    private String value;
    
    @ManyToOne(
            targetEntity = PaymentOption.class,
            fetch = FetchType.LAZY,
            optional = false,
            cascade = {CascadeType.PERSIST, CascadeType.MERGE})
    @JoinColumn(name = "transfer_id")
    private Transfer transfer;

}
