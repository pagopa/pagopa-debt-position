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
import it.gov.pagopa.debtposition.repository.specification.PaymentPositionByDueDate;
import it.gov.pagopa.debtposition.repository.specification.PaymentPositionByIUPD;
import it.gov.pagopa.debtposition.repository.specification.PaymentPositionByOrganizationFiscalCode;
import it.gov.pagopa.debtposition.util.CommonUtil;
import it.gov.pagopa.debtposition.validation.DebtPositionValidation;
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
import java.util.Optional;


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


    public PaymentPosition create(@NotNull PaymentPosition debtPosition, @NotBlank String organizationFiscalCode) {

        final String ERROR_CREATION_LOG_MSG = "Error during debt position creation: %s";

        try {

            // verifico la correttezza dei dati in input
            DebtPositionValidation.checkPaymentPositionInputDataAccurancy(debtPosition);

            // predispongo i dati ad uso interno prima dell'aggiornamento
            LocalDateTime currentDate = LocalDateTime.now(ZoneOffset.UTC);
            LocalDateTime minDueDate = debtPosition.getPaymentOption().stream().map(PaymentOption::getDueDate).min(LocalDateTime::compareTo).orElse(currentDate);
            LocalDateTime maxDueDate = debtPosition.getPaymentOption().stream().map(PaymentOption::getDueDate).max(LocalDateTime::compareTo).orElse(currentDate);
            debtPosition.setMinDueDate(minDueDate);
            debtPosition.setMaxDueDate(maxDueDate);
            debtPosition.setInsertedDate(currentDate);
            debtPosition.setLastUpdatedDate(currentDate);
            debtPosition.setOrganizationFiscalCode(organizationFiscalCode);
            debtPosition.setStatus(DebtPositionStatus.DRAFT);
            for (PaymentOption po : debtPosition.getPaymentOption()) {
                po.setOrganizationFiscalCode(organizationFiscalCode);
                po.setInsertedDate(currentDate);
                po.setLastUpdatedDate(currentDate);
                po.setStatus(PaymentOptionStatus.PO_UNPAID);
                for (Transfer t : po.getTransfer()) {
                    t.setIuv(po.getIuv());
                    t.setOrganizationFiscalCode(organizationFiscalCode);
                    t.setInsertedDate(currentDate);
                    t.setLastUpdatedDate(currentDate);
                    t.setStatus(TransferStatus.T_UNREPORTED);
                }
            }

            // Inserisco la posizione debitoria
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
        } catch (Exception e) {
            log.error(String.format(ERROR_CREATION_LOG_MSG, e.getMessage()), e);
            throw new AppException(AppError.DEBT_POSITION_CREATION_FAILED, organizationFiscalCode);
        }
    }

    public PaymentPosition getDebtPositionByIUPD(String organizationFiscalCode,
                                                 String iupd) {

//        Specification<PaymentPosition> spec = Specification.where(
//                new PaymentPositionByOrganizationFiscalCode(organizationFiscalCode)
//                        .and(new PaymentPositionByIUPD(iupd))
//        );
//
//        Optional<PaymentPosition> pp = paymentPositionRepository.findOne(spec);
        Optional<PaymentPosition> pp = paymentPositionRepository.findPaymentPositionByCodeAndIupd(organizationFiscalCode, iupd);
        if (pp.isEmpty()) {
            throw new AppException(AppError.DEBT_POSITION_NOT_FOUND, organizationFiscalCode, iupd);
        }

        return pp.get();
    }

    public Page<PaymentPosition> getOrganizationDebtPositions(@Positive Integer limit, @Positive Integer pageNum, FilterAndOrder filterAndOrder) {
    	
    	// verifico la correttezza dell'intervallo di date fornito
        DebtPositionValidation.checkDatesInterval(filterAndOrder.getFilter().getDueDateFrom(), filterAndOrder.getFilter().getDueDateTo(), Integer.parseInt(maxDaysInterval));

        Pageable pageable = PageRequest.of(pageNum, limit, CommonUtil.getSort(filterAndOrder));

        Specification<PaymentPosition> spec = Specification.where(
                new PaymentPositionByOrganizationFiscalCode(filterAndOrder.getFilter().getOrganizationFiscalCode())
                        .and(new PaymentPositionByDueDate(
                                filterAndOrder.getFilter().getDueDateFrom(),
                                filterAndOrder.getFilter().getDueDateTo())));

        return paymentPositionRepository.findAll(spec, pageable);

    }


    @Transactional
    public void delete(@NotBlank String organizationFiscalCode, @NotBlank String iupd) {
        PaymentPosition ppToRemove = this.getDebtPositionByIUPD(organizationFiscalCode, iupd);
        if (DebtPositionStatus.getPaymentPosAlreadyPaidStatus().contains(ppToRemove.getStatus())) {
            throw new AppException(AppError.DEBT_POSITION_PAYMENT_FOUND, organizationFiscalCode, iupd);
        }
        paymentPositionRepository.delete(ppToRemove);
    }


    @Transactional
    public PaymentPosition update(@NotNull @Valid PaymentPositionModel paymentPositionModel, @NotBlank String organizationFiscalCode) {

        final String ERROR_UPDATE_LOG_MSG = "Error during debt position update: %s";

        PaymentPosition ppToUpdate = this.getDebtPositionByIUPD(organizationFiscalCode, paymentPositionModel.getIupd());

        if (DebtPositionStatus.getPaymentPosNotUpdatableStatus().contains(ppToUpdate.getStatus())) {
            throw new AppException(AppError.DEBT_POSITION_NOT_UPDATABLE, organizationFiscalCode, paymentPositionModel.getIupd());
        }

        try {

            // flip model to entity
            ppToUpdate.getPaymentOption().clear();
            modelMapper.map(paymentPositionModel, ppToUpdate);

            // verifico la correttezza dei dati in input
            DebtPositionValidation.checkPaymentPositionInputDataAccurancy(ppToUpdate);

            // predispongo i dati ad uso interno prima dell'aggiornamento
            LocalDateTime currentDate = LocalDateTime.now(ZoneOffset.UTC);
            LocalDateTime minDueDate = ppToUpdate.getPaymentOption().stream().map(PaymentOption::getDueDate).min(LocalDateTime::compareTo).orElse(currentDate);
            LocalDateTime maxDueDate = ppToUpdate.getPaymentOption().stream().map(PaymentOption::getDueDate).max(LocalDateTime::compareTo).orElse(currentDate);
            ppToUpdate.setMinDueDate(minDueDate);
            ppToUpdate.setMaxDueDate(maxDueDate);
            ppToUpdate.setLastUpdatedDate(currentDate);
            ppToUpdate.setPublishDate(null);
            ppToUpdate.setStatus(DebtPositionStatus.DRAFT);
            for (PaymentOption po : ppToUpdate.getPaymentOption()) {
                po.setOrganizationFiscalCode(organizationFiscalCode);
                po.setInsertedDate(ppToUpdate.getInsertedDate());
                po.setLastUpdatedDate(currentDate);
                po.setStatus(PaymentOptionStatus.PO_UNPAID);
                for (Transfer t : po.getTransfer()) {
                    t.setIuv(po.getIuv());
                    t.setOrganizationFiscalCode(organizationFiscalCode);
                    t.setInsertedDate(ppToUpdate.getInsertedDate());
                    t.setLastUpdatedDate(currentDate);
                    t.setStatus(TransferStatus.T_UNREPORTED);
                }
            }

            return paymentPositionRepository.saveAndFlush(ppToUpdate);

        } catch (ValidationException e) {
            throw new AppException(AppError.DEBT_POSITION_REQUEST_DATA_ERROR, e.getMessage());
        } catch (Exception e) {
            log.error(String.format(ERROR_UPDATE_LOG_MSG, e.getMessage()), e);
            throw new AppException(AppError.DEBT_POSITION_UPDATE_FAILED, organizationFiscalCode);
        }
    }
}
