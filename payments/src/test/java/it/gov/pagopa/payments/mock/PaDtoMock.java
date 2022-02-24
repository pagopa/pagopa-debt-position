package it.gov.pagopa.payments.mock;

import it.gov.pagopa.commons.model.PaDto;

public class PaDtoMock {
	public final static PaDto getMock() {
		PaDto paDto = new PaDto();

		paDto.setId(0L);
		paDto.setCodAmm("p_mt");
		paDto.setDesAmm("Provincia di Matera");
		paDto.setTipologiaIstat("Province e loro Consorzi e Associazioni");
		paDto.setCodiceFiscale("80000970774");
		paDto.setCodiceInterbancario("ABCDE");
		paDto.setIndirizzo("Via Ridola, 60");
		paDto.setComune("Matera");
		paDto.setCap("75100");
		paDto.setProvincia("MT");
		paDto.setEmailCertificata("provincia.matera@cert.ruparbasilicata.it");
		paDto.setSitoIstituzionale("www.provincia.matera.it");

		return paDto;

	}
}
