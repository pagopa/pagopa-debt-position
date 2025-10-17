package it.gov.pagopa.debtposition.service.config;

import it.gov.pagopa.debtposition.model.config.Notice;
import it.gov.pagopa.debtposition.repository.InstallmentRepository;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import jakarta.transaction.Transactional;
import java.util.List;

@Service
@Slf4j
public class SendConfigurator {
    private final InstallmentRepository installmentRepository;

    @Autowired
    public SendConfigurator(InstallmentRepository installmentRepository) {
        this.installmentRepository = installmentRepository;
    }

    @Transactional
    public int updateSendSync(List<Notice> notices) {
        int updated = 0;
        for (Notice n: notices) {
            updated += installmentRepository.updateInstallmentSendSync(n.getOrganizationFiscalCode(), n.getNav());
        }
        return updated;
    }
}
