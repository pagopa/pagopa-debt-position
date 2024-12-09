package it.gov.pagopa.debtposition.controller.odp;

import it.gov.pagopa.debtposition.model.enumeration.DebtPositionStatus;
import it.gov.pagopa.debtposition.model.enumeration.ServiceType;
import it.gov.pagopa.debtposition.model.filterandorder.Order;
import it.gov.pagopa.debtposition.model.odp.PaymentOptionModelOdp;
import it.gov.pagopa.debtposition.model.odp.PaymentPositionModelOdp;
import it.gov.pagopa.debtposition.model.odp.PaymentPositionsInfoOdp;
import it.gov.pagopa.debtposition.model.odp.response.PaymentPositionModelResponseOdp;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.Sort;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;

import java.time.LocalDate;


@Controller
@Slf4j
public class DebtPositionControllerOdp implements IDebtPositionControllerOdp {

    @Override
    public ResponseEntity<PaymentPositionModelOdp> createDebtPosition(String organizationFiscalCode,
                                                                      PaymentOptionModelOdp paymentPositionModel,
                                                                      boolean toPublish, String segregationCodes,
                                                                      ServiceType serviceType) {
        return null;
    }

    @Override
    public ResponseEntity<PaymentPositionsInfoOdp> getOrganizationDebtPositions(String organizationFiscalCode, Integer limit, Integer page, LocalDate dueDateFrom, LocalDate dueDateTo, LocalDate paymentDateFrom, LocalDate paymentDateTo, DebtPositionStatus status, Order.PaymentPositionOrder orderBy, Sort.Direction ordering, String segregationCodes) {
        return null;
    }

    @Override
    public ResponseEntity<PaymentPositionModelResponseOdp> getOrganizationDebtPositionByIUPD(String organizationFiscalCode, String iupd, String segregationCodes) {
        return null;
    }

    @Override
    public ResponseEntity<PaymentPositionModelOdp> updateDebtPosition(String organizationFiscalCode, String iupd, PaymentPositionModelOdp paymentPositionModel, boolean toPublish, String segregationCodes) {
        return null;
    }
}
