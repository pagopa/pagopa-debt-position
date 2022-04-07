package it.gov.pagopa.payments.model;

import java.util.ArrayList;

import com.microsoft.azure.storage.ResultContinuation;
import com.microsoft.azure.storage.ResultSegment;

import lombok.Data;
import lombok.EqualsAndHashCode;

@Data
@EqualsAndHashCode(callSuper=false)
public class PaymentsResultSegment<Object> extends ResultSegment<Object>{
	
	private int pageNumber;
	private ResultSegment resultSegment; 
	
	public PaymentsResultSegment(ArrayList<Object> results, Integer pageSize, ResultContinuation token) {
		super(results, pageSize, token);
		// TODO Auto-generated constructor stub
	}

	


}
