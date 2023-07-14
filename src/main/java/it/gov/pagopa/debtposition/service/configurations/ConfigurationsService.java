package it.gov.pagopa.debtposition.service.configurations;

import java.time.LocalDate;
import java.util.Collections;
import java.util.List;

import javax.validation.constraints.NotNull;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import it.gov.pagopa.debtposition.model.payments.OrganizationModelQueryBean;
import it.gov.pagopa.debtposition.repository.PaymentPositionRepository;


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
}
