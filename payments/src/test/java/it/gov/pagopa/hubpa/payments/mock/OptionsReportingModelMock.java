package it.gov.pagopa.hubpa.payments.mock;

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import it.gov.pagopa.hubpa.payments.model.OptionsReportingModel;

public class OptionsReportingModelMock {
	public final static OptionsReportingModel getMock() {
		OptionsReportingModel mock = new OptionsReportingModel();
		List<String> options = new ArrayList<>(Arrays.asList("op1", "op2", "op3"));
		mock.setDateFlow(LocalDate.now());
		mock.setIdFlow("idflow1234");
		mock.setNotificationCodes(options);
		return mock;
	}
}
