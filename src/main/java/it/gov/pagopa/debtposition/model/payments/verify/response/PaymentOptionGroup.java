package it.gov.pagopa.debtposition.model.payments.verify.response;

import java.time.LocalDateTime;
import java.util.List;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter 
@Setter 
@NoArgsConstructor 
@AllArgsConstructor 
@Builder
public class PaymentOptionGroup {
	private String description;           
	private Integer numberOfInstallments; 
	private Long amount;                  
	private LocalDateTime dueDate;        
	private LocalDateTime validFrom;      
	private String status;                
	private String statusReason;          
	private Boolean allCCP;              
	private List<InstallmentSummary> installments;
}
