package it.gov.pagopa.debtposition.controller.pd.crud.api.v2;

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
}
