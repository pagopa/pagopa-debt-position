package it.gov.pagopa.debtposition.controller.pd.actions.api.impl;

import it.gov.pagopa.debtposition.controller.pd.actions.api.IDebtPositionActionsController;
import it.gov.pagopa.debtposition.entity.PaymentPosition;
import it.gov.pagopa.debtposition.exception.AppError;
import it.gov.pagopa.debtposition.exception.AppException;
import it.gov.pagopa.debtposition.model.pd.PaymentPositionModel;
import it.gov.pagopa.debtposition.service.pd.actions.PaymentPositionActionsService;
import lombok.extern.slf4j.Slf4j;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;

import java.util.ArrayList;
import java.util.Arrays;


@Controller
@Slf4j
public class DebtPositionActionsController implements IDebtPositionActionsController {

    private static final String LOG_BASE_HEADER_INFO = "[RequestMethod: %s] - [ClassMethod: %s] - [MethodParamsToLog: %s]";
    private static final String LOG_BASE_PARAMS_DETAIL = "organizationFiscalCode= %s; iupd= %s";
    @Autowired
    private ModelMapper modelMapper;
    @Autowired
    private PaymentPositionActionsService paymentPositionActionsService;
  

    @Override
    public ResponseEntity<PaymentPositionModel> publishDebtPosition(String organizationFiscalCode, String iupd, String segregationCodes) {
        log.info(String.format(LOG_BASE_HEADER_INFO, "POST", "publishDebtPosition", String.format(LOG_BASE_PARAMS_DETAIL, organizationFiscalCode, iupd)));

        ArrayList<String> segCodes = segregationCodes != null ? new ArrayList<>(Arrays.asList(segregationCodes.split(","))) : null;
        PaymentPosition publishedDebtPos = paymentPositionActionsService.publish(organizationFiscalCode, iupd, segCodes);
        if (null != publishedDebtPos) {
        	PaymentPositionModel paymentPosition = modelMapper.map(publishedDebtPos, PaymentPositionModel.class);
            return new ResponseEntity<>(paymentPosition, HttpStatus.OK);
        }

        throw new AppException(AppError.DEBT_POSITION_PUBLISH_FAILED, organizationFiscalCode);
    }

    @Override
    public ResponseEntity<PaymentPositionModel> invalidateDebtPosition(String organizationFiscalCode, String iupd, String segregationCodes) {
        log.info(String.format(LOG_BASE_HEADER_INFO, "POST", "invalidateDebtPosition", String.format(LOG_BASE_PARAMS_DETAIL, organizationFiscalCode, iupd)));

        ArrayList<String> segCodes = segregationCodes != null ? new ArrayList<>(Arrays.asList(segregationCodes.split(","))) : null;
        PaymentPosition invalidatedDebtPos = paymentPositionActionsService.invalidate(organizationFiscalCode, iupd, segCodes);
        if (null != invalidatedDebtPos) {
        	PaymentPositionModel paymentPosition = modelMapper.map(invalidatedDebtPos, PaymentPositionModel.class);
            return new ResponseEntity<>(paymentPosition, HttpStatus.OK);
        }

        throw new AppException(AppError.DEBT_POSITION_INVALIDATE_FAILED, organizationFiscalCode);
    }

}
