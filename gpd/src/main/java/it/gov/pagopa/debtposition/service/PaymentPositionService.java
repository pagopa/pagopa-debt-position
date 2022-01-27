package it.gov.pagopa.debtposition.service;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;

import it.gov.pagopa.debtposition.entity.PaymentPosition;
import it.gov.pagopa.debtposition.exception.AppError;
import it.gov.pagopa.debtposition.exception.AppException;
import it.gov.pagopa.debtposition.repository.PaymentPositionRepository;
import it.gov.pagopa.debtposition.repository.specification.PaymentPositionByIUPD;
import it.gov.pagopa.debtposition.repository.specification.PaymentPositionByOrganizationFiscalCode;



@Service
public class PaymentPositionService {
    
    @Autowired
    private PaymentPositionRepository paymentPositionRepository;
    
    public List<PaymentPosition> getDebtPositionByIUPD (String organizationFiscalCode,
            String iupd) {
        
        Specification<PaymentPosition> spec = Specification.where(
                new PaymentPositionByOrganizationFiscalCode(organizationFiscalCode)
                .and(new PaymentPositionByIUPD(iupd))
                );
        
        List<PaymentPosition> ppList = paymentPositionRepository.findAll(spec);
        if (ppList.isEmpty()) {
            throw new AppException(AppError.DEBT_POSITION_NOT_FOUND, organizationFiscalCode, iupd);
        }
        
        return ppList;
    }
}
