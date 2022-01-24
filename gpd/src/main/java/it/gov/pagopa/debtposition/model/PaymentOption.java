package it.gov.pagopa.debtposition.model;

import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

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
import javax.persistence.OneToMany;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;
import javax.persistence.UniqueConstraint;
import javax.validation.constraints.NotNull;

import it.gov.pagopa.debtposition.model.enumeration.PaymentOptionStatus;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;


/**
 * @author aacitelli
 * 
 * JPA Entity
 *
 */

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Entity
@Table (name = "payment_option", uniqueConstraints={
	    @UniqueConstraint(name = "UniquePaymentOpt", columnNames = {"iuv", "organization_fiscal_code"})
	    })
public class PaymentOption implements Serializable {

    
    
    /**
	 * generated serialVersionUID
	 */
	private static final long serialVersionUID = -2800191377721368418L;

	@Id
	@GeneratedValue (strategy = GenerationType.SEQUENCE, generator="PAYMENT_OPT_SEQ")
	@SequenceGenerator(name="PAYMENT_OPT_SEQ", sequenceName="PAYMENT_OPT_SEQ", allocationSize=1)
	private long id;
    
	@NotNull
    private String iuv;
	@NotNull
    @Column (name="organization_fiscal_code")
    private String organizationFiscalCode;
    
    
	@NotNull
    private long amount;
    private String description;
    @NotNull
    @Column (name = "is_partial_payment")
    private Boolean isPartialPayment;
    @NotNull
    @Column (name="due_date")
    private LocalDateTime dueDate;
    @Column (name="retention_date")
    private LocalDateTime retentionDate;
    @Column (name="payment_date")
    private LocalDateTime paymentDate;
    @Column (name="reporting_date")
    private LocalDateTime reportingDate;
    @Column (name="payment_method")
    private String paymentMethod;
    private long fee;
    @Column (name="psp_company")
    private String pspCompany;
    @Column (name="receipt_id")
    private String idReceipt;
    @Column (name="flow_reporting_id")
    private String idFlowReporting;
    @Column (nullable = false)
    @Enumerated(EnumType.STRING)
    private PaymentOptionStatus status = PaymentOptionStatus.PO_UNPAID;    
    
    @ManyToOne(targetEntity = PaymentPosition.class, fetch = FetchType.LAZY, optional = false, cascade = {CascadeType.PERSIST})
    @JoinColumn(name = "payment_position_id")
    private PaymentPosition paymentPosition;
    
    
    @OneToMany(targetEntity = Transfer.class, fetch = FetchType.LAZY, mappedBy = "paymentOption", cascade = CascadeType.ALL, orphanRemoval = true)
	private List<Transfer> transfer = new ArrayList<>();
    
    public void addTransfers(Transfer trans) {
    	transfer.add(trans);
        trans.setPaymentOption(this);
    }

    public void removeTransfers(Transfer trans) {
    	transfer.remove(trans);
        trans.setPaymentOption(null);
    }
    

}
