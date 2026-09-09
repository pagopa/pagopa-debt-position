package it.gov.pagopa.debtposition.service.pd.actions;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import it.gov.pagopa.debtposition.DebtPositionApplication;
import it.gov.pagopa.debtposition.entity.PaymentPosition;
import it.gov.pagopa.debtposition.exception.AppError;
import it.gov.pagopa.debtposition.exception.AppException;
import it.gov.pagopa.debtposition.model.enumeration.DebtPositionStatus;
import it.gov.pagopa.debtposition.repository.PaymentPositionRepository;
import it.gov.pagopa.debtposition.service.pd.crud.PaymentPositionCRUDService;
import java.util.List;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.http.HttpStatus;
import org.springframework.orm.ObjectOptimisticLockingFailureException;
import org.springframework.test.context.bean.override.mockito.MockitoBean;

@SpringBootTest(classes = DebtPositionApplication.class)
class PaymentPositionActionsServiceTest {

	@Autowired
	private PaymentPositionActionsService paymentPositionActionsService;

	@MockitoBean
	private PaymentPositionCRUDService paymentPositionCRUDService;

	@MockitoBean
	private PaymentPositionRepository paymentPositionRepository;

	@Test
	void publish_ObjectOptimisticLockingFailureException_throwsConcurrentPublishFailure() {
		String organizationFiscalCode = "02406911202";
		String iupd = "IUPD-1";

		PaymentPosition paymentPosition = new PaymentPosition();
		paymentPosition.setId(1L);
		paymentPosition.setOrganizationFiscalCode(organizationFiscalCode);
		paymentPosition.setIupd(iupd);
		paymentPosition.setStatus(DebtPositionStatus.DRAFT);
		paymentPosition.setPaymentOption(List.of());

		when(paymentPositionCRUDService.getDebtPositionByIUPD(organizationFiscalCode, iupd, null))
				.thenReturn(paymentPosition);

		when(paymentPositionRepository.saveAndFlush(any(PaymentPosition.class)))
				.thenThrow(new ObjectOptimisticLockingFailureException(PaymentPosition.class, paymentPosition.getId()));

		AppException exception = assertThrows(AppException.class,
				() -> paymentPositionActionsService.publish(organizationFiscalCode, iupd, null));

		assertEquals(AppError.DEBT_POSITION_CONCURRENT_PUBLISH_FAILURE, exception.getAppError());

		assertEquals(HttpStatus.CONFLICT, exception.getHttpStatus());

		verify(paymentPositionRepository).saveAndFlush(paymentPosition);
	}

	@Test
	void invalidate_ObjectOptimisticLockingFailureException_throwsConcurrentInvalidateFailure() {
		String organizationFiscalCode = "02406911202";
		String iupd = "IUPD-1";

		PaymentPosition paymentPosition = new PaymentPosition();
		paymentPosition.setId(1L);
		paymentPosition.setOrganizationFiscalCode(organizationFiscalCode);
		paymentPosition.setIupd(iupd);
		paymentPosition.setStatus(DebtPositionStatus.VALID);
		paymentPosition.setPaymentOption(List.of());

		when(paymentPositionCRUDService.getDebtPositionByIUPD(organizationFiscalCode, iupd, null))
				.thenReturn(paymentPosition);

		when(paymentPositionRepository.saveAndFlush(any(PaymentPosition.class)))
				.thenThrow(new ObjectOptimisticLockingFailureException(PaymentPosition.class, paymentPosition.getId()));

		AppException exception = assertThrows(AppException.class,
				() -> paymentPositionActionsService.invalidate(organizationFiscalCode, iupd, null));

		assertEquals(AppError.DEBT_POSITION_CONCURRENT_INVALIDATE_FAILURE, exception.getAppError());

		assertEquals(HttpStatus.CONFLICT, exception.getHttpStatus());

		verify(paymentPositionRepository).saveAndFlush(paymentPosition);
	}
}