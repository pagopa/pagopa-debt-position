package it.gov.pagopa.debtposition.controller.pd.crud.api.v3;

import it.gov.pagopa.debtposition.entity.PaymentPosition;
import it.gov.pagopa.debtposition.exception.AppError;
import it.gov.pagopa.debtposition.exception.AppException;
import it.gov.pagopa.debtposition.model.enumeration.DebtPositionStatusV3;
import it.gov.pagopa.debtposition.model.enumeration.ServiceType;
import it.gov.pagopa.debtposition.model.filterandorder.Order;
import it.gov.pagopa.debtposition.model.v3.PaymentPositionModelV3;
import it.gov.pagopa.debtposition.model.v3.PaymentPositionsInfoV3;
import it.gov.pagopa.debtposition.model.v3.response.PaymentPositionModelResponseV3;
import it.gov.pagopa.debtposition.service.pd.crud.PaymentPositionCRUDService;
import lombok.extern.slf4j.Slf4j;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Sort;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Arrays;

import static it.gov.pagopa.debtposition.util.Constants.CREATE_ACTION;


@Controller
@Slf4j
public class DebtPositionControllerV3 implements IDebtPositionControllerV3 {
    @Autowired
    private ModelMapper modelMapper;
    @Autowired
    private PaymentPositionCRUDService paymentPositionService;

    @Override
    public ResponseEntity<PaymentPositionModelV3> createDebtPosition(String organizationFiscalCode, PaymentPositionModelV3 paymentPositionModelV3, boolean toPublish, String segregationCodes, ServiceType serviceType) {
        // flip model to entity
        PaymentPosition debtPosition = modelMapper.map(paymentPositionModelV3, PaymentPosition.class);
        debtPosition.setServiceType(serviceType);

        ArrayList<String> segCodes = segregationCodes != null ? new ArrayList<>(Arrays.asList(segregationCodes.split(","))) : null;
        PaymentPosition created = paymentPositionService.create(debtPosition, organizationFiscalCode, toPublish, segCodes, CREATE_ACTION);

        if (null == created)
            throw new AppException(AppError.DEBT_POSITION_CREATION_FAILED, organizationFiscalCode);

        return new ResponseEntity<>(paymentPositionModelV3, HttpStatus.CREATED);
    }

    @Override
    public ResponseEntity<PaymentPositionsInfoV3> getOrganizationDebtPositions(String organizationFiscalCode, Integer limit, Integer page, LocalDate dueDateFrom, LocalDate dueDateTo, LocalDate paymentDateFrom, LocalDate paymentDateTo, DebtPositionStatusV3 status, Order.PaymentPositionOrder orderBy, Sort.Direction ordering, String segregationCodes) {
        return null;
    }

    @Override
    public ResponseEntity<PaymentPositionModelResponseV3> getOrganizationDebtPositionByIUPD(String organizationFiscalCode, String iupd, String segregationCodes) {
        ArrayList<String> segCodes = segregationCodes != null ? new ArrayList<>(Arrays.asList(segregationCodes.split(","))) : null;
        // flip entity to model
        PaymentPositionModelResponseV3 paymentPositionResponse = modelMapper.map(
                paymentPositionService.getDebtPositionByIUPD(organizationFiscalCode, iupd, segCodes),
                PaymentPositionModelResponseV3.class);

        return new ResponseEntity<>(paymentPositionResponse, HttpStatus.OK);
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
