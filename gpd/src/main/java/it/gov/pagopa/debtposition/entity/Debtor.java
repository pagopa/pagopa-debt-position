/**
 * 
 */
package it.gov.pagopa.debtposition.entity;

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
import javax.persistence.OneToMany;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;
import javax.persistence.UniqueConstraint;
import javax.validation.constraints.NotNull;

import it.gov.pagopa.debtposition.model.enumeration.Type;
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
@Table (name = "debtor", uniqueConstraints={
        @UniqueConstraint(name = "UniqueDebtor", columnNames = {"type", "fiscal_code"})
})
public class Debtor implements Serializable {
    /**
     * generated serialVersionUID
     */
    private static final long serialVersionUID = -1005793427472544064L;

    @Id
    @GeneratedValue (strategy = GenerationType.SEQUENCE, generator="DEBTOR_SEQ")
    @SequenceGenerator(name="DEBTOR_SEQ", sequenceName="DEBTOR_SEQ", allocationSize=1)
    private long id;

    @NotNull
    @Enumerated(EnumType.STRING)
    private Type type;
    @NotNull
    @Column (name="fiscal_code")
    private String fiscalCode;

    @NotNull
    private String fullName;
    @Column (name="street_name")
    private String streetName;
    @Column (name="civic_number")
    private String civicNumber;
    @Column (name="postal_code")
    private String postalCode;
    private String city;
    private String province;
    private String region;
    private String country;
    private String email;
    private String phone;
    @NotNull
    @Column (name="inserted_date")
    private LocalDateTime insertedDate;
    @NotNull
    @Column (name="last_updated_date")
    private LocalDateTime lastUpdatedDate;

    @OneToMany(targetEntity = PaymentPosition.class, fetch = FetchType.LAZY, mappedBy = "debtor", cascade = CascadeType.ALL)
    private List<PaymentPosition> paymentPosition = new ArrayList<>();

    
    public void addPaymentPosition(PaymentPosition paymentPos) {
        paymentPosition.add(paymentPos);
        paymentPos.setDebtor(this);
    }

    public void removePaymentPosition(PaymentPosition paymentPos) {
        paymentPosition.remove(paymentPos);
        paymentPos.setDebtor(null);
    }
}
