package it.gov.pagopa.payments.exception;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;
import org.mockito.Mockito;
import org.springframework.http.HttpInputMessage;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.http.converter.HttpMessageNotReadableException;
import org.springframework.web.bind.MissingServletRequestParameterException;
import org.springframework.web.context.request.WebRequest;

import it.gov.pagopa.payments.model.ProblemJson;

class ErrorHandlerTest {

	@Test
	void handleError() {
		HttpInputMessage inputMessage=Mockito.mock(HttpInputMessage.class);
		ErrorHandler eh = new ErrorHandler();
		ResponseEntity<Object>  re = eh.handleHttpMessageNotReadable(new HttpMessageNotReadableException ("error", inputMessage), null, null, null);
		assertEquals(HttpStatus.BAD_REQUEST, re.getStatusCode());
		
		re = eh.handleMissingServletRequestParameter(new MissingServletRequestParameterException ("name", "type"), null, null, null);
		assertEquals(HttpStatus.BAD_REQUEST, re.getStatusCode());
		
		WebRequest request=Mockito.mock(WebRequest.class);
		AppException e = new AppException(AppError.RECEIPT_NOT_FOUND, "org", "iuv");
		ResponseEntity<ProblemJson> r = eh.handleAppException(e, request);
		assertEquals(HttpStatus.NOT_FOUND, r.getStatusCode());
		
		r = eh.handleGenericException(new Exception("generic error"), request);
		assertEquals(HttpStatus.INTERNAL_SERVER_ERROR, r.getStatusCode());
	}
}
