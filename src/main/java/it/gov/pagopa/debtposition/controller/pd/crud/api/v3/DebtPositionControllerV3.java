package it.gov.pagopa.debtposition.controller.pd.crud.api.v3;

import it.gov.pagopa.debtposition.model.enumeration.DebtPositionStatusV3;
import it.gov.pagopa.debtposition.model.enumeration.ServiceType;
import it.gov.pagopa.debtposition.model.filterandorder.Order;
import it.gov.pagopa.debtposition.model.v3.PaymentPositionModelV3;
import it.gov.pagopa.debtposition.model.v3.PaymentPositionsInfoV3;
import it.gov.pagopa.debtposition.model.v3.response.PaymentPositionModelResponseV3;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.Sort;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;

import java.time.LocalDate;


@Controller
@Slf4j
public class DebtPositionControllerV3 implements IDebtPositionControllerV3 {

    @Override
    public ResponseEntity<PaymentPositionModelV3> createDebtPosition(String organizationFiscalCode, PaymentPositionModelV3 paymentPositionModel, boolean toPublish, String segregationCodes, ServiceType serviceType) {
        return null;
    }

    @Override
    public ResponseEntity<PaymentPositionsInfoV3> getOrganizationDebtPositions(String organizationFiscalCode, Integer limit, Integer page, LocalDate dueDateFrom, LocalDate dueDateTo, LocalDate paymentDateFrom, LocalDate paymentDateTo, DebtPositionStatusV3 status, Order.PaymentPositionOrder orderBy, Sort.Direction ordering, String segregationCodes) {
        return null;
    }

    @Override
    public ResponseEntity<PaymentPositionModelResponseV3> getOrganizationDebtPositionByIUPD(String organizationFiscalCode, String iupd, String segregationCodes) {
        return null;
    }

    @Override
    public ResponseEntity<PaymentPositionModelV3> updateDebtPosition(String organizationFiscalCode, String iupd, PaymentPositionModelV3 paymentPositionModel, boolean toPublish, String segregationCodes) {
        return null;
    }

    @Override
    public ResponseEntity<String> deleteDebtPosition(String organizationFiscalCode, String iupd, String segregationCodes) {
        return null;
    }
}
