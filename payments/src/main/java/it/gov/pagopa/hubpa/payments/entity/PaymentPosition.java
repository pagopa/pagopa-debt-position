package it.gov.pagopa.hubpa.payments.entity;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.Table;

import com.fasterxml.jackson.annotation.JsonIgnore;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
@Entity
@Table(name = "payment_position")
public class PaymentPosition {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(name = "organization_fiscal_code", nullable = false)
    private String organizationFiscalCode;
    @Column(name = "company_name", nullable = true)
    private String companyName;
    @Column(name = "office_name", nullable = true)
    private String officeName;
    @Column(name = "status", nullable = false)
    private Integer status;
    @Column(name = "description", nullable = true)
    private String description;
    @Column(name = "job_id", nullable = false)
    private Long jobId;
    @Column(name = "insert_date", nullable = false)
    private LocalDateTime insertDate;
    @Column(name = "amount", nullable = false)
    private BigDecimal amount;
    @Column(name = "information", nullable = true)
    private String information;
    @Column(name = "publish_date", nullable = true)
    private LocalDate publishDate;
    @Column(name = "total_options", nullable = false)
    private Integer totalOptions;
    @Column(name = "paid_options", nullable = false)
    private Integer paidOptions;
    @Column(name = "reported_options", nullable = false)
    private Integer reportedOptions;

    @ManyToOne(targetEntity = Debitor.class, fetch = FetchType.LAZY, cascade = {CascadeType.PERSIST})
    @JoinColumn(name = "debitor_id")
    @JsonIgnore
    private Debitor debitor;
    
    @OneToMany(targetEntity = PaymentOptions.class, fetch = FetchType.LAZY, mappedBy = "paymentPosition", cascade = CascadeType.ALL, orphanRemoval = true)
    private List<PaymentOptions> paymentOptions = new ArrayList<>();

    public void addPaymentOptions(PaymentOptions paymentOpt) {
	paymentOptions.add(paymentOpt);
	paymentOpt.setPaymentPosition(this);
    }

    public void removePaymentOptions(PaymentOptions paymentOpt) {
	paymentOptions.remove(paymentOpt);
	paymentOpt.setPaymentPosition(null);
    }
}
