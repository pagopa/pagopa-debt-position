package it.gov.pagopa.debtposition.controller.pd.actions.api.v3;


import it.gov.pagopa.debtposition.model.v3.PaymentPositionModelV3;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;


@Controller
@Slf4j
public class DebtPositionActionsControllerV3 implements IDebtPositionActionsControllerV3 {

    @Override
    public ResponseEntity<PaymentPositionModelV3> publishDebtPosition(String organizationFiscalCode, String iupd, String segregationCodes) {
        return null;
    }
}
