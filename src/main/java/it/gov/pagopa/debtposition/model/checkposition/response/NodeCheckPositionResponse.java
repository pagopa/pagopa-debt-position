package it.gov.pagopa.debtposition.model.checkposition.response;

import java.io.Serializable;

import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
public class NodeCheckPositionResponse implements Serializable{
	/**
	 * generated serialVersionUID
	 */
	private static final long serialVersionUID = 5395824544388480006L;
	
	private String outcome;

}
