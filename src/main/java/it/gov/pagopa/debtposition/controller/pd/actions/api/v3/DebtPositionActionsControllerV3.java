package it.gov.pagopa.debtposition.controller.pd.actions.api.v3;

import it.gov.pagopa.debtposition.entity.PaymentPosition;
import it.gov.pagopa.debtposition.exception.AppError;
import it.gov.pagopa.debtposition.exception.AppException;
import it.gov.pagopa.debtposition.model.v3.PaymentPositionModelV3;
import it.gov.pagopa.debtposition.service.pd.actions.PaymentPositionActionsService;
import java.util.ArrayList;
import java.util.Arrays;
import lombok.extern.slf4j.Slf4j;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;

@Controller
@Slf4j
public class DebtPositionActionsControllerV3 implements IDebtPositionActionsControllerV3 {

  @Autowired private ModelMapper modelMapper;
  @Autowired private PaymentPositionActionsService paymentPositionActionsService;

  @Override
  public ResponseEntity<PaymentPositionModelV3> publishDebtPosition(
      String organizationFiscalCode, String iupd, String segregationCodes) {
    ArrayList<String> segCodes =
        segregationCodes != null
            ? new ArrayList<>(Arrays.asList(segregationCodes.split(",")))
            : null;
    PaymentPosition ppPublished =
        paymentPositionActionsService.publish(organizationFiscalCode, iupd, segCodes);
    if (null != ppPublished) {
      PaymentPositionModelV3 ppModelV3 = modelMapper.map(ppPublished, PaymentPositionModelV3.class);
      return new ResponseEntity<>(ppModelV3, HttpStatus.OK);
    }

    throw new AppException(AppError.DEBT_POSITION_PUBLISH_FAILED, organizationFiscalCode);
  }
}
