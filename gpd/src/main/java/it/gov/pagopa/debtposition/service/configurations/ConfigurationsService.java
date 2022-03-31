package it.gov.pagopa.debtposition.service.configurations;

import it.gov.pagopa.debtposition.entity.PaymentPosition;
import it.gov.pagopa.debtposition.exception.AppError;
import it.gov.pagopa.debtposition.exception.AppException;
import it.gov.pagopa.debtposition.model.payments.OrganizationModelQueryBean;
import it.gov.pagopa.debtposition.repository.PaymentPositionRepository;
import it.gov.pagopa.debtposition.repository.specification.PaymentPositionByOrganizationFiscalCode;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;

import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
import java.time.LocalDate;
import java.util.Collections;
import java.util.List;


@Service
public class ConfigurationsService {

    @Autowired
    private PaymentPositionRepository paymentPositionRepository;


    public List<OrganizationModelQueryBean> getOrganizationsToAdd(@NotNull LocalDate since) {
        return paymentPositionRepository.findDistinctOrganizationsByInsertedDate(since.atStartOfDay());
    }

    public List<OrganizationModelQueryBean> getOrganizationsToDelete(@NotNull LocalDate since) {
        paymentPositionRepository.findDistinctOrganizationsByInsertedDate(since.atStartOfDay());
        return Collections.emptyList();
    }

    public void checkOrganization(@NotEmpty String organizationFiscalCode) {
        Specification<PaymentPosition> spec = Specification.where(
                new PaymentPositionByOrganizationFiscalCode(organizationFiscalCode));
        List<PaymentPosition> ppList = paymentPositionRepository.findAll(spec);
        if (ppList.isEmpty()) {
            throw new AppException(AppError.ORGANIZATION_NOT_FOUND, organizationFiscalCode);
        }
    }


}
