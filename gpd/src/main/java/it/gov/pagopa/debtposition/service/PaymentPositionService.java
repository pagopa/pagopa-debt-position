package it.gov.pagopa.debtposition.service;

import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.util.Optional;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Positive;

import org.hibernate.exception.ConstraintViolationException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataIntegrityViolationException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;

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
import it.gov.pagopa.debtposition.repository.PaymentPositionRepository;
import it.gov.pagopa.debtposition.repository.specification.PaymentPositionByDueDate;
import it.gov.pagopa.debtposition.repository.specification.PaymentPositionByIUPD;
import it.gov.pagopa.debtposition.repository.specification.PaymentPositionByOrganizationFiscalCode;
import it.gov.pagopa.debtposition.util.CommonUtil;
import it.gov.pagopa.debtposition.validation.DebtPositionValidation;
import lombok.extern.slf4j.Slf4j;


@Service
@Slf4j
public class PaymentPositionService {
	
	private static final String UNIQUE_KEY_VIOLATION = "23505";
	@Autowired
	private PaymentPositionRepository paymentPositionRepository;
	
	public PaymentPosition create (@NotNull PaymentPosition debtPosition, @NotBlank String organizationFiscalCode) {
        PaymentPosition savedDebtPosition = null;
        try {
            
            // verifico la correttezza dei dati in input
            DebtPositionValidation.checkPaymentPositionInputDataAccurancy(debtPosition);
            
            // predispongo l'entity per l'inserimento
            LocalDateTime currentDate = LocalDateTime.now(ZoneOffset.UTC);
            debtPosition.setInsertedDate(currentDate);
            debtPosition.setLastUpdatedDate(currentDate);
            debtPosition.setOrganizationFiscalCode(organizationFiscalCode);
            debtPosition.setStatus(DebtPositionStatus.DRAFT);
            for (PaymentOption po : debtPosition.getPaymentOption()) {
                po.setOrganizationFiscalCode(organizationFiscalCode);
                po.setInsertedDate(currentDate);
                po.setLastUpdatedDate(currentDate);
                po.setStatus(PaymentOptionStatus.PO_UNPAID);
                po.setPaymentPosition(debtPosition);
                for (Transfer t: po.getTransfer()) {
                    t.setOrganizationFiscalCode(organizationFiscalCode);
                    t.setInsertedDate(currentDate);
                    t.setLastUpdatedDate(currentDate);
                    t.setStatus(TransferStatus.T_UNREPORTED);
                    t.setPaymentOption(po);
                }
            }
            
            // Inserisco la posizione debitoria
            savedDebtPosition = paymentPositionRepository.saveAndFlush(debtPosition);
        } catch (DataIntegrityViolationException e) {
            log.error("Error during debt position creation:"+e.getMessage(), e);
            if (e.getCause() instanceof ConstraintViolationException) {
                String sqlState = ((ConstraintViolationException) e.getCause()).getSQLState();
                if (sqlState.equals(UNIQUE_KEY_VIOLATION)) {
                    throw new AppException(AppError.DEBT_POSITION_ALREADY_EXIST, organizationFiscalCode);
                }
            }
        } catch (ValidationException e) {
            throw new AppException(AppError.DEBT_POSITION_INPUT_DATA_ERROR, e.getMessage());
        } catch (Exception e) {
            log.error("Error during debt position creation:"+e.getMessage(), e);
            throw new AppException(AppError.DEBT_POSITION_CREATION_FAILED, organizationFiscalCode);
        }
        return savedDebtPosition;
    }
	
	public PaymentPosition getDebtPositionByIUPD (String organizationFiscalCode,
			String iupd) {
		
		Specification<PaymentPosition> spec = Specification.where(
				new PaymentPositionByOrganizationFiscalCode(organizationFiscalCode)
				.and(new PaymentPositionByIUPD(iupd))
				);
		
		Optional<PaymentPosition> pp = paymentPositionRepository.findOne(spec);
		if (pp.isEmpty()) {
			throw new AppException(AppError.DEBT_POSITION_NOT_FOUND, organizationFiscalCode, iupd);
		}
		
		return pp.get();
	}
	
	public Page<PaymentPosition> getOrganizationDebtPositions (@Positive Integer limit, @Positive Integer pageNum, FilterAndOrder filterAndOrder){
		
		Pageable pageable = PageRequest.of(pageNum, limit, CommonUtil.getSort(filterAndOrder));
		
		Specification<PaymentPosition> spec = Specification.where(
                new PaymentPositionByOrganizationFiscalCode(filterAndOrder.getFilter().getOrganizationFiscalCode())
                .and(new PaymentPositionByDueDate(
                        filterAndOrder.getFilter().getDueDateFrom(),
                        filterAndOrder.getFilter().getDueDateTo())));
			
		return paymentPositionRepository.findAll(spec, pageable);

	}
	
	public void deleteDebtPosition(@NotBlank String organizationFiscalCode, @NotBlank String iupd) {
		PaymentPosition ppToRemove = this.getDebtPositionByIUPD(organizationFiscalCode, iupd);
		if (DebtPositionStatus.getPaymentPosAlreadyPaidStatus().contains(ppToRemove.getStatus())){
			throw new AppException(AppError.DEBT_POSITION_PAYMENT_FOUND, organizationFiscalCode, iupd);
		}
		paymentPositionRepository.delete(ppToRemove);
	}
}
