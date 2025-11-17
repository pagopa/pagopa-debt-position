package it.gov.pagopa.debtposition.model.payments.verify.response;

import java.time.LocalDateTime;

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
public class InstallmentSummary {
	private String nav;
	private String iuv;
	private Long amount;
	private String description;
	private LocalDateTime dueDate;
	private LocalDateTime validFrom;
	private String status;        
	private String statusReason;
}
