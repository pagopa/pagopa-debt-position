package it.gov.pagopa.hubpa.payments.mock;

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import it.gov.pagopa.hubpa.payments.model.PublishModel;

public class PublishModelMock {
    public final static PublishModel getMock() {
	PublishModel mock = new PublishModel();
	List<Long> ids=new ArrayList<>(Arrays.asList(1l,2l,3l));
	mock.setIds(ids);
	mock.setPublishDate(LocalDate.now());

	return mock;
    }
}
