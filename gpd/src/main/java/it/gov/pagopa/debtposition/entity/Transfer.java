package it.gov.pagopa.debtposition.entity;

import java.io.Serializable;
import java.time.LocalDateTime;

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
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;
import javax.persistence.UniqueConstraint;
import javax.validation.constraints.NotNull;

import it.gov.pagopa.debtposition.model.enumeration.TransferStatus;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

/**
 * @author aacitelli
 * 
 * JPA Entity
 *
 */

@Builder (toBuilder = true)
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Entity
@Table (name = "transfer",
        uniqueConstraints = {
	      @UniqueConstraint(
	          name = "UniqueTransfer",
	          columnNames = {"iuv", "organization_fiscal_code", "transfer_id"})
	    })
public class Transfer implements Serializable {

	/**
	 * generated serialVersionUID
	 */
	private static final long serialVersionUID = -886970813082991109L;

	@Id
	@GeneratedValue (strategy = GenerationType.SEQUENCE, generator="TRANSFER_SEQ")
	@SequenceGenerator(name="TRANSFER_SEQ", sequenceName="TRANSFER_SEQ", allocationSize=1)
	private Long id;
	
	@NotNull
	@Column (name="organization_fiscal_code")
	private String organizationFiscalCode;
	@NotNull
    @Column (name="transfer_id")
    private String idTransfer;
	@NotNull 
	private String iuv;
	@NotNull
    private long amount;
	@NotNull
	@Column (name="remittance_information")
	private String remittanceInformation; // causale
	@NotNull
	private String category; // taxonomy
	@NotNull
	private String iban;
	@Column (name="postal_iban")
	private String postalIban;
	@NotNull
	@Column (name="inserted_date")
	private LocalDateTime insertedDate;
	@NotNull
    @Enumerated(EnumType.STRING)
    private TransferStatus status; 
	@NotNull
	@Column (name="last_updated_date")
	private LocalDateTime lastUpdatedDate;
	
	
	@ManyToOne(targetEntity = PaymentOption.class, fetch = FetchType.LAZY, optional = false, cascade = {CascadeType.PERSIST, CascadeType.MERGE})
    @JoinColumn(name = "payment_option_id")
    private PaymentOption paymentOption;

}
