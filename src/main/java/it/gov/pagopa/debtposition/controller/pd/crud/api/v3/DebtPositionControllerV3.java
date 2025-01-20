package it.gov.pagopa.debtposition.controller.pd.crud.api.v3;

import static it.gov.pagopa.debtposition.util.Constants.CREATE_ACTION;
import static it.gov.pagopa.debtposition.util.Constants.UPDATE_ACTION;

import it.gov.pagopa.debtposition.entity.PaymentPosition;
import it.gov.pagopa.debtposition.exception.AppError;
import it.gov.pagopa.debtposition.exception.AppException;
import it.gov.pagopa.debtposition.model.enumeration.DebtPositionStatus;
import it.gov.pagopa.debtposition.model.enumeration.DebtPositionStatusV3;
import it.gov.pagopa.debtposition.model.enumeration.ServiceType;
import it.gov.pagopa.debtposition.model.filterandorder.Filter;
import it.gov.pagopa.debtposition.model.filterandorder.FilterAndOrder;
import it.gov.pagopa.debtposition.model.filterandorder.Order;
import it.gov.pagopa.debtposition.model.v3.PaymentPositionModelV3;
import it.gov.pagopa.debtposition.model.v3.PaymentPositionsInfoV3;
import it.gov.pagopa.debtposition.model.v3.response.PaymentPositionModelResponseV3;
import it.gov.pagopa.debtposition.service.pd.crud.PaymentPositionCRUDService;
import it.gov.pagopa.debtposition.util.CommonUtil;
import it.gov.pagopa.debtposition.util.Constants;
import it.gov.pagopa.debtposition.util.ObjectMapperUtils;
import java.time.LocalDate;
import java.time.LocalTime;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import lombok.extern.slf4j.Slf4j;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Sort;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;

@Controller
@Slf4j
public class DebtPositionControllerV3 implements IDebtPositionControllerV3 {
  private static final String LOG_BASE_HEADER_INFO =
      "[RequestMethod: %s] - [ClassMethod: %s] - [MethodParamsToLog: %s]";
  private static final String LOG_BASE_PARAMS_DETAIL = "organizationFiscalCode= %s; iupd= %s";
  private static final String IUPD_VALIDATION_ERROR =
      "IUPD mistmatch error: path variable IUPD [%s] and request body IUPD [%s] must be the same";

  private final ModelMapper modelMapper;
  private final PaymentPositionCRUDService paymentPositionService;

  @Autowired
  public DebtPositionControllerV3(
      ModelMapper modelMapper, PaymentPositionCRUDService paymentPositionService) {
    this.modelMapper = modelMapper;
    this.paymentPositionService = paymentPositionService;
  }

  @Override
  public ResponseEntity<PaymentPositionModelV3> createDebtPosition(
      String organizationFiscalCode,
      PaymentPositionModelV3 ppInputModelV3,
      boolean toPublish,
      String segregationCodes,
      ServiceType serviceType) {
    // flip model to entity
    PaymentPosition debtPosition = modelMapper.map(ppInputModelV3, PaymentPosition.class);
    debtPosition.setServiceType(serviceType);

    ArrayList<String> segCodes =
        segregationCodes != null
            ? new ArrayList<>(Arrays.asList(segregationCodes.split(",")))
            : null;
    PaymentPosition ppCreated =
        paymentPositionService.create(
            debtPosition, organizationFiscalCode, toPublish, segCodes, CREATE_ACTION);

    if (null == ppCreated)
      throw new AppException(AppError.DEBT_POSITION_CREATION_FAILED, organizationFiscalCode);

    PaymentPositionModelV3 ppModelV3 = modelMapper.map(ppCreated, PaymentPositionModelV3.class);
    return new ResponseEntity<>(ppModelV3, HttpStatus.CREATED);
  }

  @Override
  public ResponseEntity<PaymentPositionsInfoV3> getOrganizationDebtPositions(
      String organizationFiscalCode,
      Integer limit,
      Integer page,
      LocalDate dueDateFrom,
      LocalDate dueDateTo,
      LocalDate paymentDateFrom,
      LocalDate paymentDateTo,
      DebtPositionStatusV3 status,
      Order.PaymentPositionOrder orderBy,
      Sort.Direction ordering,
      String segregationCodes) {
    ArrayList<String> segCodesList =
        segregationCodes != null
            ? new ArrayList<>(Arrays.asList(segregationCodes.split(",")))
            : null;

    // Create filter and order object
    FilterAndOrder filterOrder =
        FilterAndOrder.builder()
            .filter(
                Filter.builder()
                    .organizationFiscalCode(organizationFiscalCode)
                    .dueDateFrom(dueDateFrom != null ? dueDateFrom.atStartOfDay() : null)
                    .dueDateTo(dueDateTo != null ? dueDateTo.atTime(LocalTime.MAX) : null)
                    .paymentDateFrom(
                        paymentDateFrom != null ? paymentDateFrom.atStartOfDay() : null)
                    .paymentDateTo(
                        paymentDateTo != null ? paymentDateTo.atTime(LocalTime.MAX) : null)
                    .status(status != null ? DebtPositionStatus.valueOf(status.name()) : null)
                    .segregationCodes(segCodesList)
                    .build())
            .order(Order.builder().orderBy(orderBy).ordering(ordering).build())
            .build();

    Page<PaymentPosition> pagePP =
        paymentPositionService.getOrganizationDebtPositions(limit, page, filterOrder);

    // flip entity to model
    List<PaymentPositionModelResponseV3> ppResponseList =
        ObjectMapperUtils.mapAll(pagePP.toList(), PaymentPositionModelResponseV3.class);

    return new ResponseEntity<>(
        PaymentPositionsInfoV3.builder()
            .ppBaseResponseList(ppResponseList)
            .pageInfo(CommonUtil.buildPageInfo(pagePP))
            .build(),
        HttpStatus.OK);
  }

  @Override
  public ResponseEntity<PaymentPositionModelResponseV3> getOrganizationDebtPositionByIUPD(
      String organizationFiscalCode, String iupd, String segregationCodes) {
    ArrayList<String> segCodes =
        segregationCodes != null
            ? new ArrayList<>(Arrays.asList(segregationCodes.split(",")))
            : null;
    // flip entity to model
    PaymentPositionModelResponseV3 paymentPositionResponse =
        modelMapper.map(
            paymentPositionService.getDebtPositionByIUPD(organizationFiscalCode, iupd, segCodes),
            PaymentPositionModelResponseV3.class);

    return new ResponseEntity<>(paymentPositionResponse, HttpStatus.OK);
  }

  @Override
  public ResponseEntity<PaymentPositionModelV3> updateDebtPosition(
      String organizationFiscalCode,
      String iupd,
      PaymentPositionModelV3 paymentPositionModel,
      boolean toPublish,
      String segregationCodes) {
    if (!paymentPositionModel.getIupd().equals(iupd)) {
      log.error(
          "{} : {}",
          String.format(
              LOG_BASE_HEADER_INFO,
              "PUT",
              "updateDebtPosition",
              String.format(
                  LOG_BASE_PARAMS_DETAIL,
                  CommonUtil.sanitize(organizationFiscalCode),
                  CommonUtil.sanitize(iupd))),
          String.format(
              IUPD_VALIDATION_ERROR,
              CommonUtil.sanitize(iupd),
              CommonUtil.sanitize(paymentPositionModel.getIupd())));
      throw new AppException(
          AppError.DEBT_POSITION_REQUEST_DATA_ERROR,
          String.format(IUPD_VALIDATION_ERROR, iupd, paymentPositionModel.getIupd()));
    }

    ArrayList<String> segCodes =
        segregationCodes != null
            ? new ArrayList<>(Arrays.asList(segregationCodes.split(",")))
            : null;
    PaymentPosition ppUpdated =
        paymentPositionService.update(
            paymentPositionModel, organizationFiscalCode, toPublish, segCodes, UPDATE_ACTION);

    if (null != ppUpdated) {
      PaymentPositionModelV3 ppModelV3 = modelMapper.map(ppUpdated, PaymentPositionModelV3.class);
      return new ResponseEntity<>(ppModelV3, HttpStatus.OK);
    }

    throw new AppException(AppError.DEBT_POSITION_UPDATE_FAILED, organizationFiscalCode);
  }

  @Override
  public ResponseEntity<String> deleteDebtPosition(
      String organizationFiscalCode, String iupd, String segregationCodes) {
    ArrayList<String> segCodes =
        segregationCodes != null
            ? new ArrayList<>(Arrays.asList(segregationCodes.split(",")))
            : null;
    paymentPositionService.delete(organizationFiscalCode, iupd, segCodes);
    return new ResponseEntity<>(Constants.DEBT_POSITION_DELETED, HttpStatus.OK);
  }
}
