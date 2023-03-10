package it.gov.pagopa.debtposition.controller.pd.crud.api.impl;

import it.gov.pagopa.debtposition.config.ExclusiveParamGroup;
import it.gov.pagopa.debtposition.controller.pd.crud.api.IDebtPositionController;
import it.gov.pagopa.debtposition.entity.PaymentPosition;
import it.gov.pagopa.debtposition.exception.AppError;
import it.gov.pagopa.debtposition.exception.AppException;
import it.gov.pagopa.debtposition.model.enumeration.DebtPositionStatus;
import it.gov.pagopa.debtposition.model.filterandorder.Filter;
import it.gov.pagopa.debtposition.model.filterandorder.FilterAndOrder;
import it.gov.pagopa.debtposition.model.filterandorder.Order;
import it.gov.pagopa.debtposition.model.filterandorder.Order.PaymentPositionOrder;
import it.gov.pagopa.debtposition.model.pd.PaymentPositionModel;
import it.gov.pagopa.debtposition.model.pd.PaymentPositionsInfo;
import it.gov.pagopa.debtposition.model.pd.response.PaymentPositionModelBaseResponse;
import it.gov.pagopa.debtposition.service.pd.crud.PaymentPositionCRUDService;
import it.gov.pagopa.debtposition.util.CommonUtil;
import it.gov.pagopa.debtposition.util.Constants;
import it.gov.pagopa.debtposition.util.ObjectMapperUtils;
import lombok.extern.slf4j.Slf4j;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Sort.Direction;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestParam;

import javax.validation.Valid;
import javax.validation.constraints.Positive;
import java.time.LocalDate;
import java.time.LocalTime;
import java.util.List;


@Controller
@Slf4j
public class DebtPositionController implements IDebtPositionController {

    private static final String LOG_BASE_HEADER_INFO = "[RequestMethod: %s] - [ClassMethod: %s] - [MethodParamsToLog: %s]";
    private static final String LOG_BASE_PARAMS_DETAIL = "organizationFiscalCode= %s; iupd= %s";
    @Autowired
    private ModelMapper modelMapper;
    @Autowired
    private PaymentPositionCRUDService paymentPositionService;

    @Override
    public ResponseEntity<PaymentPositionModel> createDebtPosition(String organizationFiscalCode,
                                                                   @Valid PaymentPositionModel paymentPositionModel,
                                                                   boolean toPublish) {
        log.info(String.format(LOG_BASE_HEADER_INFO, "POST", "createDebtPosition", String.format(LOG_BASE_PARAMS_DETAIL, organizationFiscalCode, paymentPositionModel.getIupd())));

        // flip model to entity
        PaymentPosition debtPosition = modelMapper.map(paymentPositionModel, PaymentPosition.class);

        PaymentPosition createdDebtPos = paymentPositionService.create(debtPosition, organizationFiscalCode, toPublish);

        if (null != createdDebtPos) {
            return new ResponseEntity<>(modelMapper.map(createdDebtPos, PaymentPositionModel.class), HttpStatus.CREATED);
        }

        throw new AppException(AppError.DEBT_POSITION_CREATION_FAILED, organizationFiscalCode);
    }

    @Override
    public ResponseEntity<PaymentPositionModelBaseResponse> getOrganizationDebtPositionByIUPD(String organizationFiscalCode,
                                                                                              String iupd) {
        log.info(String.format(LOG_BASE_HEADER_INFO, "GET", "getOrganizationDebtPositionByIUPD", String.format(LOG_BASE_PARAMS_DETAIL, organizationFiscalCode, iupd)));

        // flip entity to model
        PaymentPositionModelBaseResponse paymentPositionResponse = ObjectMapperUtils.map(
                paymentPositionService.getDebtPositionByIUPD(organizationFiscalCode, iupd),
                PaymentPositionModelBaseResponse.class);

        return new ResponseEntity<>(paymentPositionResponse, HttpStatus.OK);
    }

    @Override
    @ExclusiveParamGroup(firstGroup = {"due_date_to", "due_date_from"}, secondGroup = {"payment_date_from", "payment_date_to"})
    public ResponseEntity<PaymentPositionsInfo> getOrganizationDebtPositions(String organizationFiscalCode,
                                                                             @Positive Integer limit, @Positive Integer page, LocalDate dueDateFrom,
                                                                             LocalDate dueDateTo, LocalDate paymentDateFrom, LocalDate paymentDateTo,
                                                                             DebtPositionStatus status, PaymentPositionOrder orderBy, Direction ordering) {
        log.info(String.format(LOG_BASE_HEADER_INFO, "GET", "getOrganizationDebtPositions", String.format(LOG_BASE_PARAMS_DETAIL, organizationFiscalCode, "N/A")));

        // Create filter and order object
        FilterAndOrder filterOrder = FilterAndOrder.builder()
                .filter(Filter.builder()
                        .organizationFiscalCode(organizationFiscalCode)
                        .dueDateFrom(dueDateFrom != null ? dueDateFrom.atStartOfDay() : null)
                        .dueDateTo(dueDateTo != null ? dueDateTo.atTime(LocalTime.MAX) : null)
                        .paymentDateFrom(paymentDateFrom != null ? paymentDateFrom.atStartOfDay() : null)
                        .paymentDateTo(paymentDateTo != null ? paymentDateTo.atTime(LocalTime.MAX) : null)
                        .status(status)
                        .build())
                .order(Order.builder()
                        .orderBy(orderBy)
                        .ordering(ordering)
                        .build())
                .build();

        Page<PaymentPosition> pagePP = paymentPositionService.getOrganizationDebtPositions(limit, page, filterOrder);

        // flip entity to model
        List<PaymentPositionModelBaseResponse> ppResponseList = ObjectMapperUtils.mapAll(
                pagePP.toList(),
                PaymentPositionModelBaseResponse.class);

        return new ResponseEntity<>(PaymentPositionsInfo.builder()
                .ppBaseResponseList(ppResponseList)
                .pageInfo(CommonUtil.buildPageInfo(pagePP))
                .build(),
                HttpStatus.OK);
    }

    @Override
    public ResponseEntity<String> deleteDebtPosition(String organizationFiscalCode, String iupd) {
        log.info(String.format(LOG_BASE_HEADER_INFO, "DELETE", "deleteDebtPosition", String.format(LOG_BASE_PARAMS_DETAIL, organizationFiscalCode, iupd)));

        paymentPositionService.delete(organizationFiscalCode, iupd);
        return new ResponseEntity<>(Constants.DEBT_POSITION_DELETED, HttpStatus.OK);
    }

    @Override
    public ResponseEntity<PaymentPositionModel> updateDebtPosition(String organizationFiscalCode, String iupd,
                                                                   @Valid PaymentPositionModel paymentPositionModel) {
        final String IUPD_VALIDATION_ERROR = "IUPD mistmatch error: path variable IUPD [%s] and request body IUPD [%s] must be the same";

        log.info(String.format(LOG_BASE_HEADER_INFO, "PUT", "updateDebtPosition", String.format(LOG_BASE_PARAMS_DETAIL, organizationFiscalCode, iupd)));
        // verifico la congruenza di dati tra lo iupd path variable e lo iupd nel request body
        if (!paymentPositionModel.getIupd().equals(iupd)) {
            log.error(String.format(LOG_BASE_HEADER_INFO, "PUT", "updateDebtPosition", String.format(LOG_BASE_PARAMS_DETAIL, organizationFiscalCode, iupd)) +
                    " : " + String.format(IUPD_VALIDATION_ERROR, iupd, paymentPositionModel.getIupd()));

            throw new AppException(AppError.DEBT_POSITION_REQUEST_DATA_ERROR, String.format(IUPD_VALIDATION_ERROR, iupd, paymentPositionModel.getIupd()));
        }

        PaymentPosition updatedDebtPos = paymentPositionService.update(paymentPositionModel, organizationFiscalCode);

        if (null != updatedDebtPos) {
            return new ResponseEntity<>(modelMapper.map(updatedDebtPos, PaymentPositionModel.class), HttpStatus.OK);
        }

        throw new AppException(AppError.DEBT_POSITION_UPDATE_FAILED, organizationFiscalCode);

    }

}
