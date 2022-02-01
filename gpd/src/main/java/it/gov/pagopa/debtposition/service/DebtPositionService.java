package it.gov.pagopa.debtposition.service;

import java.time.LocalDateTime;
import java.time.ZoneOffset;

import org.hibernate.exception.ConstraintViolationException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataIntegrityViolationException;
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
import it.gov.pagopa.debtposition.repository.DebtPositionRepository;
import it.gov.pagopa.debtposition.validation.DebtPositionValidation;
import lombok.extern.slf4j.Slf4j;


@Service
@Slf4j
public class DebtPositionService {

    private static final String UNIQUE_KEY_VIOLATION = "23505";
    @Autowired
    private DebtPositionRepository debtPositionRepository;
    
    
    public PaymentPosition create (PaymentPosition debtPosition, String organizationFiscalCode) {
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
            savedDebtPosition = debtPositionRepository.saveAndFlush(debtPosition);
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
}
