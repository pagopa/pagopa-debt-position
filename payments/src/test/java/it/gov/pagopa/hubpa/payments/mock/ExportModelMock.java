package it.gov.pagopa.hubpa.payments.mock;

import java.util.ArrayList;
import java.util.List;

import it.gov.pagopa.hubpa.payments.model.ExportModel;

public class ExportModelMock {
    public final static ExportModel getMock() {
	ExportModel mock = new ExportModel();
	List<Long> ids=new ArrayList<>();
	ids.add(1l);
	ids.add(2l);
	
	mock.setIds(ids);
	mock.setIsMailing(true);
	
	return mock;
    }
    public final static ExportModel getMock2() {
	ExportModel mock = getMock();
	mock.setIsMailing(false);
	
	return mock;
    }
}
