package it.gov.pagopa.debtposition.controller.pd.crud.api.v1;

import static it.gov.pagopa.debtposition.util.Constants.CREATE_ACTION;
import static it.gov.pagopa.debtposition.util.Constants.UPDATE_ACTION;

import it.gov.pagopa.debtposition.config.ExclusiveParamGroup;
import it.gov.pagopa.debtposition.entity.PaymentPosition;
import it.gov.pagopa.debtposition.exception.AppError;
import it.gov.pagopa.debtposition.exception.AppException;
import it.gov.pagopa.debtposition.model.enumeration.DebtPositionStatus;
import it.gov.pagopa.debtposition.model.enumeration.ServiceType;
import it.gov.pagopa.debtposition.model.filterandorder.Filter;
import it.gov.pagopa.debtposition.model.filterandorder.FilterAndOrder;
import it.gov.pagopa.debtposition.model.filterandorder.Order;
import it.gov.pagopa.debtposition.model.filterandorder.Order.PaymentPositionOrder;
import it.gov.pagopa.debtposition.model.pd.MultipleIUPDModel;
import it.gov.pagopa.debtposition.model.pd.MultiplePaymentPositionModel;
import it.gov.pagopa.debtposition.model.pd.PaymentPositionModel;
import it.gov.pagopa.debtposition.model.pd.PaymentPositionsInfo;
import it.gov.pagopa.debtposition.model.pd.UpdateTransferIbanMassiveModel;
import it.gov.pagopa.debtposition.model.pd.response.PaymentPositionModelBaseResponse;
import it.gov.pagopa.debtposition.model.pd.response.UpdateTransferIbanMassiveResponse;
import it.gov.pagopa.debtposition.service.pd.crud.PaymentPositionCRUDService;
import it.gov.pagopa.debtposition.util.CommonUtil;
import it.gov.pagopa.debtposition.util.Constants;
import it.gov.pagopa.debtposition.util.ObjectMapperUtils;
import java.time.LocalDate;
import java.time.LocalTime;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;
import javax.validation.Valid;
import javax.validation.constraints.Pattern;
import lombok.extern.slf4j.Slf4j;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Sort.Direction;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.util.CollectionUtils;

@Controller
@Slf4j
public class DebtPositionController implements IDebtPositionController {

  private static final String LOG_BASE_HEADER_INFO =
      "[RequestMethod: %s] - [ClassMethod: %s] - [MethodParamsToLog: %s]";
  private static final String LOG_BASE_PARAMS_DETAIL = "organizationFiscalCode= %s; iupd= %s";
  private final ModelMapper modelMapper;
  private final PaymentPositionCRUDService paymentPositionService;

  @Autowired
  public DebtPositionController(
      ModelMapper modelMapper, PaymentPositionCRUDService paymentPositionService) {
    this.modelMapper = modelMapper;
    this.paymentPositionService = paymentPositionService;
  }

  @Override
  public ResponseEntity<PaymentPositionModel> createDebtPosition(
      String organizationFiscalCode,
      PaymentPositionModel paymentPositionModel,
      boolean toPublish,
      String segregationCodes,
      ServiceType serviceType) {
    log.debug(
        String.format(
            LOG_BASE_HEADER_INFO,
            "POST",
            "createDebtPosition",
            String.format(
                LOG_BASE_PARAMS_DETAIL,
                CommonUtil.sanitize(organizationFiscalCode),
                CommonUtil.sanitize(paymentPositionModel.getIupd()))));

    // flip model to entity
    PaymentPosition debtPosition = modelMapper.map(paymentPositionModel, PaymentPosition.class);
    debtPosition.setServiceType(serviceType);

    ArrayList<String> segCodes =
        segregationCodes != null
            ? new ArrayList<>(Arrays.asList(segregationCodes.split(",")))
            : null;
    PaymentPosition createdDebtPos =
        paymentPositionService.create(
            debtPosition, organizationFiscalCode, toPublish, segCodes, CREATE_ACTION);

    if (null != createdDebtPos) {
      PaymentPositionModel paymentPosition =
          ObjectMapperUtils.map(createdDebtPos, PaymentPositionModel.class);
      return new ResponseEntity<>(paymentPosition, HttpStatus.CREATED);
    }

    throw new AppException(AppError.DEBT_POSITION_CREATION_FAILED, organizationFiscalCode);
  }

  @Override
  @ExclusiveParamGroup(
      firstGroup = {"due_date_to", "due_date_from"},
      secondGroup = {"payment_date_from", "payment_date_to"})
  public ResponseEntity<PaymentPositionsInfo> getOrganizationDebtPositions(
      String organizationFiscalCode,
      Integer limit,
      Integer page,
      LocalDate dueDateFrom,
      LocalDate dueDateTo,
      LocalDate paymentDateFrom,
      LocalDate paymentDateTo,
      DebtPositionStatus status,
      PaymentPositionOrder orderBy,
      Direction ordering,
      String segregationCodes) {
    log.debug(
        String.format(
            LOG_BASE_HEADER_INFO,
            "GET",
            "getOrganizationDebtPositions",
            String.format(
                LOG_BASE_PARAMS_DETAIL, CommonUtil.sanitize(organizationFiscalCode), "N/A")));

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
                    .status(status)
                    .segregationCodes(segCodesList)
                    .build())
            .order(Order.builder().orderBy(orderBy).ordering(ordering).build())
            .build();

    Page<PaymentPosition> pagePP =
        paymentPositionService.getOrganizationDebtPositions(limit, page, filterOrder);

    // flip entity to model
    List<PaymentPositionModelBaseResponse> ppResponseList =
        ObjectMapperUtils.mapAll(pagePP.toList(), PaymentPositionModelBaseResponse.class);

    return new ResponseEntity<>(
        PaymentPositionsInfo.builder()
            .ppBaseResponseList(ppResponseList)
            .pageInfo(CommonUtil.buildPageInfo(pagePP))
            .build(),
        HttpStatus.OK);
  }

  @Override
  public ResponseEntity<PaymentPositionModelBaseResponse> getOrganizationDebtPositionByIUPD(
      @Pattern(regexp = "[\\w*\\h-]+") String organizationFiscalCode,
      @Pattern(regexp = "[\\w*\\h-]+") String iupd,
      @Valid @Pattern(regexp = "\\d{2}(,\\d{2})*") String segregationCodes) {
    log.debug(
        String.format(
            LOG_BASE_HEADER_INFO,
            "GET",
            "getOrganizationDebtPositionByIUPD",
            String.format(
                LOG_BASE_PARAMS_DETAIL,
                CommonUtil.sanitize(organizationFiscalCode),
                CommonUtil.sanitize(iupd))));

    ArrayList<String> segCodes =
        segregationCodes != null
            ? new ArrayList<>(Arrays.asList(segregationCodes.split(",")))
            : null;
    // flip entity to model
    PaymentPositionModelBaseResponse paymentPositionResponse =
        ObjectMapperUtils.map(
            paymentPositionService.getDebtPositionByIUPD(organizationFiscalCode, iupd, segCodes),
            PaymentPositionModelBaseResponse.class);

    return new ResponseEntity<>(paymentPositionResponse, HttpStatus.OK);
  }

  @Override
  public ResponseEntity<String> deleteDebtPosition(
      @Pattern(regexp = "[\\w*\\h-]+") String organizationFiscalCode,
      @Pattern(regexp = "[\\w*\\h-]+") String iupd,
      @Valid @Pattern(regexp = "\\d{2}(,\\d{2})*") String segregationCodes) {
    log.debug(
        String.format(
            LOG_BASE_HEADER_INFO,
            "DELETE",
            "deleteDebtPosition",
            String.format(
                LOG_BASE_PARAMS_DETAIL,
                CommonUtil.sanitize(organizationFiscalCode),
                CommonUtil.sanitize(iupd))));

    ArrayList<String> segCodes =
        segregationCodes != null
            ? new ArrayList<>(Arrays.asList(segregationCodes.split(",")))
            : null;
    paymentPositionService.delete(organizationFiscalCode, iupd, segCodes);
    return new ResponseEntity<>(Constants.DEBT_POSITION_DELETED, HttpStatus.OK);
  }

  @Override
  public ResponseEntity<PaymentPositionModel> updateDebtPosition(
      String organizationFiscalCode,
      String iupd,
      PaymentPositionModel paymentPositionModel,
      boolean toPublish,
      String segregationCodes) {
    final String IUPD_VALIDATION_ERROR =
        "IUPD mistmatch error: path variable IUPD [%s] and request body IUPD [%s] must be the same";

    log.debug(
        String.format(
            LOG_BASE_HEADER_INFO,
            "PUT",
            "updateDebtPosition",
            String.format(
                LOG_BASE_PARAMS_DETAIL,
                CommonUtil.sanitize(organizationFiscalCode),
                CommonUtil.sanitize(iupd))));
    // verifico la congruenza di dati tra lo iupd path variable e lo iupd nel request body
    if (!paymentPositionModel.getIupd().equals(iupd)) {
      log.error(
          String.format(
                  LOG_BASE_HEADER_INFO,
                  "PUT",
                  "updateDebtPosition",
                  String.format(
                      LOG_BASE_PARAMS_DETAIL,
                      CommonUtil.sanitize(organizationFiscalCode),
                      CommonUtil.sanitize(iupd)))
              + " : "
              + String.format(IUPD_VALIDATION_ERROR, iupd, paymentPositionModel.getIupd()));

      throw new AppException(
          AppError.DEBT_POSITION_REQUEST_DATA_ERROR,
          String.format(IUPD_VALIDATION_ERROR, iupd, paymentPositionModel.getIupd()));
    }

    ArrayList<String> segCodes =
        segregationCodes != null
            ? new ArrayList<>(Arrays.asList(segregationCodes.split(",")))
            : null;
    PaymentPosition updatedDebtPos =
        paymentPositionService.update(
            paymentPositionModel, organizationFiscalCode, toPublish, segCodes, UPDATE_ACTION);

    if (null != updatedDebtPos) {
      PaymentPositionModel paymentPosition =
          ObjectMapperUtils.map(updatedDebtPos, PaymentPositionModel.class);
      return new ResponseEntity<>(paymentPosition, HttpStatus.OK);
    }

    throw new AppException(AppError.DEBT_POSITION_UPDATE_FAILED, organizationFiscalCode);
  }

  @Override
  public ResponseEntity<Void> createMultipleDebtPositions(
      String organizationFiscalCode,
      @Valid MultiplePaymentPositionModel multiplePaymentPositionModel,
      boolean toPublish,
      @Valid @Pattern(regexp = "\\d{2}(,\\d{2})*") String segregationCodes,
      ServiceType serviceType) {
    log.debug(
        String.format(
            LOG_BASE_HEADER_INFO,
            "POST",
            "createMultipleDebtPositions",
            String.format(
                LOG_BASE_PARAMS_DETAIL, CommonUtil.sanitize(organizationFiscalCode), "N/A")));

    // flip model to entity
    List<PaymentPosition> debtPositions =
        multiplePaymentPositionModel.getPaymentPositions().stream()
            .map(ppModel -> modelMapper.map(ppModel, PaymentPosition.class))
            .map(
                ppMappedModel -> {
                  ppMappedModel.setServiceType(serviceType);
                  return ppMappedModel;
                })
            .collect(Collectors.toList());

    ArrayList<String> segCodes =
        segregationCodes != null
            ? new ArrayList<>(Arrays.asList(segregationCodes.split(",")))
            : null;
    List<PaymentPosition> createdDebtPosList =
        paymentPositionService.createMultipleDebtPositions(
            debtPositions, organizationFiscalCode, toPublish, segCodes, CREATE_ACTION);

    if (!CollectionUtils.isEmpty(createdDebtPosList)) {
      return ResponseEntity.status(HttpStatus.CREATED).build();
    }

    throw new AppException(AppError.DEBT_POSITION_CREATION_FAILED, organizationFiscalCode);
  }

  @Override
  public ResponseEntity<Void> updateMultipleDebtPositions(
      String organizationFiscalCode,
      @Valid MultiplePaymentPositionModel multiplePaymentPositionModel,
      boolean toPublish,
      @Valid @Pattern(regexp = "\\d{2}(,\\d{2})*") String segregationCodes) {
    log.debug(
        String.format(
            LOG_BASE_HEADER_INFO,
            "PUT",
            "updateMultipleDebtPositions",
            String.format(
                LOG_BASE_PARAMS_DETAIL, CommonUtil.sanitize(organizationFiscalCode), "N/A")));

    ArrayList<String> segCodes =
        segregationCodes != null
            ? new ArrayList<>(Arrays.asList(segregationCodes.split(",")))
            : null;
    List<PaymentPosition> updatedDebtPosList =
        paymentPositionService.updateMultipleDebtPositions(
            multiplePaymentPositionModel.getPaymentPositions(),
            organizationFiscalCode,
            toPublish,
            segCodes,
            UPDATE_ACTION);

    if (!CollectionUtils.isEmpty(updatedDebtPosList)) {
      return ResponseEntity.status(HttpStatus.OK).build();
    }

    throw new AppException(AppError.DEBT_POSITION_UPDATE_FAILED, organizationFiscalCode);
  }

  @Override
  public ResponseEntity<String> deleteMultipleDebtPositions(
      @Pattern(regexp = "\\b\\w{11}\\b") String organizationFiscalCode,
      @Valid MultipleIUPDModel multipleIUPDModel,
      @Valid @Pattern(regexp = "\\d{2}(,\\d{2})*") String segregationCodes) {
    log.debug(
        String.format(
            LOG_BASE_HEADER_INFO,
            "DELETE",
            "deleteMultipleDebtPositions",
            String.format(
                LOG_BASE_PARAMS_DETAIL, CommonUtil.sanitize(organizationFiscalCode), "N/A")));

    ArrayList<String> segCodes =
        segregationCodes != null
            ? new ArrayList<>(Arrays.asList(segregationCodes.split(",")))
            : null;
    paymentPositionService.deleteMultipleDebtPositions(
        multipleIUPDModel.getPaymentPositionIUPDs(), organizationFiscalCode, segCodes);

    return new ResponseEntity<>(Constants.DEBT_POSITION_DELETED, HttpStatus.OK);
  }

  @Override
  public ResponseEntity<PaymentPositionModelBaseResponse> getDebtPositionByIUV(
      @Pattern(regexp = "\\d{11}") String organizationFiscalCode,
      @Pattern(regexp = "\\b\\w{0,35}\\b") String iuv,
      @Valid @Pattern(regexp = "\\d{2}(,\\d{2})*") String segregationCodes) {
    log.debug(
        String.format(
            LOG_BASE_HEADER_INFO,
            "GET",
            "getDebtPositionByIUV",
            String.format(
                LOG_BASE_PARAMS_DETAIL, CommonUtil.sanitize(organizationFiscalCode), iuv)));

    ArrayList<String> segCodes =
        segregationCodes != null
            ? new ArrayList<>(Arrays.asList(segregationCodes.split(",")))
            : null;
    // flip entity to model
    PaymentPositionModelBaseResponse paymentPositionResponse =
        ObjectMapperUtils.map(
            paymentPositionService.getDebtPositionByIUV(organizationFiscalCode, iuv, segCodes),
            PaymentPositionModelBaseResponse.class);

    return new ResponseEntity<>(paymentPositionResponse, HttpStatus.OK);
  }

  @Override
  public ResponseEntity<UpdateTransferIbanMassiveResponse> updateTransferIbanMassive(
      String organizationFiscalCode,
      String oldIban,
      int limit,
      UpdateTransferIbanMassiveModel updateTransferIbanMassiveModel) {

    int numberOfUpdates =
        paymentPositionService.updateTransferIbanMassive(
            organizationFiscalCode, oldIban, updateTransferIbanMassiveModel.getNewIban(), limit);

    UpdateTransferIbanMassiveResponse response =
        UpdateTransferIbanMassiveResponse.builder()
            .description(String.format("Updated IBAN on %s Transfers", numberOfUpdates))
            .updatedTransfers(numberOfUpdates)
            .build();

    return ResponseEntity.status(HttpStatus.OK.value())
        .contentType(MediaType.APPLICATION_JSON)
        .body(response);
  }
}
