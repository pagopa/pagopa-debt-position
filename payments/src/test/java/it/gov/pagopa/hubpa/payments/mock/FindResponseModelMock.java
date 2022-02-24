package it.gov.pagopa.hubpa.payments.mock;

import java.util.ArrayList;
import java.util.List;

import it.gov.pagopa.hubpa.payments.model.FindResponseModel;
import it.gov.pagopa.hubpa.payments.model.PaymentMinimalModel;

public class FindResponseModelMock {
    public final static FindResponseModel getMock() {
	FindResponseModel mock = new FindResponseModel();
	List<PaymentMinimalModel> list = new ArrayList<>();
	list.add(PaymentMinimalModelMock.getMock());
	mock.setCurrentPage(1);
	mock.setPayments(list);
	mock.setTotalItems(150l);
	mock.setTotalPages(10);

	return mock;
    }
}
