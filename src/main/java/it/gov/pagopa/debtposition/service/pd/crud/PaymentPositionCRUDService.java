package it.gov.pagopa.debtposition.service.pd.crud;

import it.gov.pagopa.debtposition.entity.PaymentOption;
import it.gov.pagopa.debtposition.entity.PaymentPosition;
import it.gov.pagopa.debtposition.entity.Transfer;
import it.gov.pagopa.debtposition.exception.AppError;
import it.gov.pagopa.debtposition.exception.AppException;
import it.gov.pagopa.debtposition.exception.ValidationException;
import it.gov.pagopa.debtposition.model.enumeration.DebtPositionStatus;
import it.gov.pagopa.debtposition.model.enumeration.PaymentOptionStatus;
import it.gov.pagopa.debtposition.model.enumeration.TransferStatus;
import it.gov.pagopa.debtposition.model.filterandorder.FilterAndOrder;
import it.gov.pagopa.debtposition.model.pd.PaymentPositionModel;
import it.gov.pagopa.debtposition.repository.PaymentPositionRepository;
import it.gov.pagopa.debtposition.repository.specification.*;
import it.gov.pagopa.debtposition.service.payments.PaymentsService;
import it.gov.pagopa.debtposition.util.CommonUtil;
import it.gov.pagopa.debtposition.util.PublishPaymentUtil;
import it.gov.pagopa.debtposition.util.DebtPositionValidation;
import lombok.extern.slf4j.Slf4j;
import org.hibernate.exception.ConstraintViolationException;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.dao.DataIntegrityViolationException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import javax.validation.Valid;
import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Positive;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.util.*;
import java.util.stream.Collectors;


@Service
@Slf4j
public class PaymentPositionCRUDService {

    private static final String UNIQUE_KEY_VIOLATION = "23505";
    @Value("${max.days.interval}")
    private String maxDaysInterval;
    @Autowired
    private PaymentPositionRepository paymentPositionRepository;
    @Autowired
    private ModelMapper modelMapper;


    public PaymentPosition create(@NotNull PaymentPosition debtPosition, @NotBlank String organizationFiscalCode, boolean toPublish, List<String> segCodes) {

        final String ERROR_CREATION_LOG_MSG = "Error during debt position creation: %s";

        if(segCodes != null && !isAuthorizedBySegregationCode(debtPosition, segCodes))
            throw new AppException(AppError.DEBT_POSITION_FORBIDDEN, organizationFiscalCode, debtPosition.getIupd());

        try {
            // verifico la correttezza dei dati in input
            DebtPositionValidation.checkPaymentPositionInputDataAccurancy(debtPosition);

            // predispongo i dati ad uso interno prima dell'aggiornamento
            LocalDateTime currentDate = LocalDateTime.now(ZoneOffset.UTC);
            LocalDateTime minDueDate = debtPosition.getPaymentOption().stream().map(PaymentOption::getDueDate).min(LocalDateTime::compareTo).orElse(currentDate);
            LocalDateTime maxDueDate = debtPosition.getPaymentOption().stream().map(PaymentOption::getDueDate).max(LocalDateTime::compareTo).orElse(currentDate);
            debtPosition.setMinDueDate(minDueDate);
            debtPosition.setMaxDueDate(maxDueDate);
            debtPosition.setInsertedDate(Objects.requireNonNullElse(debtPosition.getInsertedDate(), currentDate));
            debtPosition.setLastUpdatedDate(currentDate);
            debtPosition.setPublishDate(null);
            debtPosition.setOrganizationFiscalCode(organizationFiscalCode);
            debtPosition.setStatus(DebtPositionStatus.DRAFT); 
            
            for (PaymentOption po : debtPosition.getPaymentOption()) {
                po.setOrganizationFiscalCode(organizationFiscalCode);
                po.setInsertedDate(Objects.requireNonNullElse(debtPosition.getInsertedDate(), currentDate));
                po.setLastUpdatedDate(currentDate);
                po.setStatus(PaymentOptionStatus.PO_UNPAID);
                for (Transfer t : po.getTransfer()) {
                    t.setIuv(po.getIuv());
                    t.setOrganizationFiscalCode(Objects.requireNonNullElse(t.getOrganizationFiscalCode(), organizationFiscalCode));
                    t.setInsertedDate(Objects.requireNonNullElse(debtPosition.getInsertedDate(), currentDate));
                    t.setLastUpdatedDate(currentDate);
                    t.setStatus(TransferStatus.T_UNREPORTED);
                }
            }

            //Se la pubblicazione immediata è richiesta, si procede
            if(toPublish){
                PublishPaymentUtil.publishProcess(debtPosition);
            }

            //Inserisco (ed eventualmente pubblico)la posizione debitoria
            return paymentPositionRepository.saveAndFlush(debtPosition);

        } catch (DataIntegrityViolationException e) {
            log.error(String.format(ERROR_CREATION_LOG_MSG, e.getMessage()), e);
            if (e.getCause() instanceof ConstraintViolationException) {
                String sqlState = ((ConstraintViolationException) e.getCause()).getSQLState();
                if (sqlState.equals(UNIQUE_KEY_VIOLATION)) {
                    throw new AppException(AppError.DEBT_POSITION_UNIQUE_VIOLATION, organizationFiscalCode);
                }
            }
            throw new AppException(AppError.DEBT_POSITION_CREATION_FAILED, organizationFiscalCode);
        } catch (ValidationException e) {
            throw new AppException(AppError.DEBT_POSITION_REQUEST_DATA_ERROR, e.getMessage());
        } catch (AppException appException) {
            throw appException;
        } catch (Exception e) {
            log.error(String.format(ERROR_CREATION_LOG_MSG, e.getMessage()), e);
            throw new AppException(AppError.DEBT_POSITION_CREATION_FAILED, organizationFiscalCode);
        }
    }

    public PaymentPosition getDebtPositionByIUPD(String organizationFiscalCode,
                                                 String iupd, List<String> segCodes) {

        Specification<PaymentPosition> spec = Specification.where(
                new PaymentPositionByOrganizationFiscalCode(organizationFiscalCode)
                        .and(new PaymentPositionByIUPD(iupd))
        );

        Optional<PaymentPosition> pp = paymentPositionRepository.findOne(spec);
        if (pp.isEmpty()) {
            throw new AppException(AppError.DEBT_POSITION_NOT_FOUND, organizationFiscalCode, iupd);
        }
        if(segCodes != null && !isAuthorizedBySegregationCode(pp.get(), segCodes)) {
            throw new AppException(AppError.DEBT_POSITION_FORBIDDEN, organizationFiscalCode, iupd);
        }

        return pp.get();
    }

    public Page<PaymentPosition> getOrganizationDebtPositions(@Positive Integer limit, @Positive Integer pageNum, FilterAndOrder filterAndOrder) {

        checkAndUpdateDates(filterAndOrder);

        Pageable pageable = PageRequest.of(pageNum, limit, CommonUtil.getSort(filterAndOrder));

        Specification<PaymentPosition> paymentPositionSpecification =
                new PaymentPositionByOrganizationFiscalCode(filterAndOrder.getFilter().getOrganizationFiscalCode())
                .and(new PaymentPositionByOptionsAttribute(
                        filterAndOrder.getFilter().getDueDateFrom(),
                        filterAndOrder.getFilter().getDueDateTo(),
                        filterAndOrder.getFilter().getSegregationCodes())
                .and(new PaymentPositionByPaymentDate(
                        filterAndOrder.getFilter().getPaymentDateFrom(),
                        filterAndOrder.getFilter().getPaymentDateTo()))
                .and(new PaymentPositionByStatus(filterAndOrder.getFilter().getStatus())));

        Specification<PaymentPosition> spec = Specification.where(paymentPositionSpecification);

        return paymentPositionRepository.findAll(spec, pageable);
    }

    @Transactional
    public void delete(@NotBlank String organizationFiscalCode, @NotBlank String iupd, List<String> segregationCodes) {
        PaymentPosition ppToRemove = this.getDebtPositionByIUPD(organizationFiscalCode, iupd, segregationCodes);
        if (DebtPositionStatus.getPaymentPosAlreadyPaidStatus().contains(ppToRemove.getStatus())) {
            throw new AppException(AppError.DEBT_POSITION_PAYMENT_FOUND, organizationFiscalCode, iupd);
        }
        paymentPositionRepository.delete(ppToRemove);
    }

    @Transactional
    public PaymentPosition update(@NotNull @Valid PaymentPositionModel paymentPositionModel, @NotBlank String organizationFiscalCode, boolean toPublish,
                                  List<String> segregationCodes) {

        final String ERROR_UPDATE_LOG_MSG = "Error during debt position update: %s";

        PaymentPosition ppToUpdate = this.getDebtPositionByIUPD(organizationFiscalCode, paymentPositionModel.getIupd(), segregationCodes);

        if (DebtPositionStatus.getPaymentPosNotUpdatableStatus().contains(ppToUpdate.getStatus())) {
            throw new AppException(AppError.DEBT_POSITION_NOT_UPDATABLE, organizationFiscalCode, paymentPositionModel.getIupd());
        }

        try {
        	// flip model to entity
            List<PaymentOption> oldPaymentOptions = new ArrayList<>(ppToUpdate.getPaymentOption());
            ppToUpdate.getPaymentOption().clear();
            modelMapper.map(paymentPositionModel, ppToUpdate);

            // migrate the notification fee value (if defined) and update the amounts
            ppToUpdate = setOldNotificationFee(oldPaymentOptions, organizationFiscalCode, ppToUpdate);
            
            // check the input data
            DebtPositionValidation.checkPaymentPositionInputDataAccurancy(ppToUpdate);
            
            paymentPositionRepository.delete(ppToUpdate);
            paymentPositionRepository.flush();
            // the version is increased at each change
            ppToUpdate.setVersion(ppToUpdate.getVersion()+1);
            return this.create(ppToUpdate, organizationFiscalCode, toPublish, segregationCodes);
        	
        } catch (ValidationException e) {
            throw new AppException(AppError.DEBT_POSITION_REQUEST_DATA_ERROR, e.getMessage());
        } catch (AppException e) {
            if (AppError.PAYMENT_OPTION_NOTIFICATION_FEE_UPDATE_TRANSFER_NOT_FOUND.title.equals(e.getTitle())) {
                throw new AppException(AppError.DEBT_POSITION_UPDATE_FAILED_NO_TRANSFER_FOR_NOTIFICATION_FEE, organizationFiscalCode, ppToUpdate.getIupd());
            }
            throw e;
        } catch (Exception e) {
            log.error(String.format(ERROR_UPDATE_LOG_MSG, e.getMessage()), e);
            throw new AppException(AppError.DEBT_POSITION_UPDATE_FAILED, organizationFiscalCode);
        }
    }

    private PaymentPosition setOldNotificationFee(List<PaymentOption> oldPaymentOptions, String organizationFiscalCode, PaymentPosition paymentPosition) {
        Map<String, Long> oldPONotificationFeeMapping = oldPaymentOptions.stream().collect(Collectors.toMap(PaymentOption::getIuv, PaymentOption::getNotificationFee));
        for (PaymentOption paymentOptionModel : paymentPosition.getPaymentOption()) {
            long oldNotificationFee = Objects.requireNonNullElse(oldPONotificationFeeMapping.get(paymentOptionModel.getIuv()), 0L);
            if (oldNotificationFee != 0) {
                PaymentsService.updateAmountsWithNotificationFee(paymentOptionModel, organizationFiscalCode, oldNotificationFee);
            }
        }
        return paymentPosition;
    }

    /*
     * Checks whether the date interval submitted is appropriate. Dates are updated by adding or subtracting the maxDaysInterval
     * in cases where one and only one of the two bounds of the date interval [from, to] is missing.
     */
    public void checkAndUpdateDates(FilterAndOrder filterAndOrder) {
        List<LocalDateTime> verifiedDueDates = DebtPositionValidation.checkDatesInterval(
                filterAndOrder.getFilter().getDueDateFrom(), filterAndOrder.getFilter().getDueDateTo(), Integer.parseInt(maxDaysInterval));
        List<LocalDateTime> verifiedPaymentDates = DebtPositionValidation.checkDatesInterval(
                filterAndOrder.getFilter().getPaymentDateFrom(), filterAndOrder.getFilter().getPaymentDateTo(), Integer.parseInt(maxDaysInterval));

        filterAndOrder.getFilter().setDueDateFrom(verifiedDueDates.get(0));
        filterAndOrder.getFilter().setDueDateTo(verifiedDueDates.get(1));
        filterAndOrder.getFilter().setPaymentDateFrom(verifiedPaymentDates.get(0));
        filterAndOrder.getFilter().setPaymentDateTo(verifiedPaymentDates.get(1));
    }

    private boolean isAuthorizedBySegregationCode(PaymentPosition paymentPosition, List<String> segregationCodes) {
        // It is enough to check only one IUV of the payment position. Here it is assumed that they all have the same segregation code.
        String paymentPositionSegregationCode = paymentPosition.getPaymentOption().get(0).getIuv().substring(0,2);
        return segregationCodes.contains(paymentPositionSegregationCode);
    }
}
