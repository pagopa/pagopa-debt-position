package it.gov.pagopa.hubpa.payments.entity;

import java.util.ArrayList;
import java.util.List;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.OneToMany;
import javax.persistence.Table;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
@Entity
@Table(name = "debitor")
public class Debitor {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;
    @Column(name = "name", nullable = false)
    private String name;
    @Column(name = "surname", nullable = false)
    private String surname;
    @Column(name = "fiscalCode", nullable = false)
    private String fiscalCode;
    @Column(name = "type", nullable = false)
    private Integer type;
    @Column(name = "phone", nullable = false)
    private String phone;
    @Column(name = "address", nullable = false)
    private String address;
    @Column(name = "number", nullable = false)
    private String number;
    @Column(name = "area", nullable = false)
    private String area;
    @Column(name = "province", nullable = false)
    private String province;
    @Column(name = "cap", nullable = false)
    private String cap;
    @Column(name = "country", nullable = false)
    private String country;
    @Column(name = "email", nullable = false)
    private String email;
    @Column(name = "id_tenant", nullable = true)
    private String idTenant;
    
  
    @OneToMany(targetEntity = PaymentPosition.class, fetch = FetchType.LAZY, mappedBy = "debitor", cascade = CascadeType.ALL, orphanRemoval = true)
    private List<PaymentPosition> paymentPosition = new ArrayList<>();

    public void addPaymentPosition(PaymentPosition paymentPos) {
	paymentPosition.add(paymentPos);
	paymentPos.setDebitor(this);
    }

    public void removePaymentPosition(PaymentPosition paymentPos) {
	paymentPosition.remove(paymentPos);
	paymentPos.setDebitor(null);
    }
}
