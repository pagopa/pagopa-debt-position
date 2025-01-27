package it.gov.pagopa.debtposition.service.iban;

import it.gov.pagopa.debtposition.entity.PaymentOption;
import it.gov.pagopa.debtposition.entity.PaymentPosition;
import it.gov.pagopa.debtposition.entity.Transfer;
import it.gov.pagopa.debtposition.model.enumeration.DebtPositionStatus;
import it.gov.pagopa.debtposition.model.enumeration.PaymentOptionStatus;
import it.gov.pagopa.debtposition.repository.PaymentOptionRepository;
import it.gov.pagopa.debtposition.repository.PaymentPositionRepository;
import it.gov.pagopa.debtposition.repository.TransferRepository;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import javax.transaction.Transactional;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.util.List;

@Service
@Slf4j
public class IbanService {

    private final PaymentPositionRepository paymentPositionRepository;
    private final PaymentOptionRepository paymentOptionRepository;
    private final TransferRepository transferRepository;

    @Autowired
    public IbanService(PaymentPositionRepository paymentPositionRepository, PaymentOptionRepository paymentOptionRepository, TransferRepository transferRepository) {
        this.paymentPositionRepository = paymentPositionRepository;
        this.paymentOptionRepository = paymentOptionRepository;
        this.transferRepository = transferRepository;

    }

    // Update all Organization's IBANs on Transfer of payable PaymentPosition
    @Transactional
    public void updateIbanMassive(String organizationFiscalCode, String oldIban, String newIban) {
        // Retrieve all payment_position with organization_fiscal_code AND in status (DRAFT or PUBLISHED or VALID or PARTIALLY_PAID)
        List<PaymentPosition> ppToUpdate = paymentPositionRepository.findByOrganizationFiscalCodeAndStatusIn(organizationFiscalCode, List.of(DebtPositionStatus.DRAFT, DebtPositionStatus.PUBLISHED, DebtPositionStatus.VALID, DebtPositionStatus.PARTIALLY_PAID));

        for (PaymentPosition pp : ppToUpdate) {
            // Retrieve all payment_option with payment_position_id AND in status PO_UNPAID
            List<PaymentOption> poToUpdate = paymentOptionRepository.findByPaymentPositionIdAndStatusIn(pp.getId(), List.of(PaymentOptionStatus.PO_UNPAID));

            for (PaymentOption po : poToUpdate) {
                // Retrieve all transfer with payment_option_id
                List<Transfer> transferToUpdate = transferRepository.findByPaymentOptionId(po.getId());

                for (Transfer tr : transferToUpdate) {
                    // Update transfer iban IF iban equals oldIban
                    if (tr.getIban().equals(oldIban)) {
                        transferRepository.updateTransferIban(tr.getId(), newIban, LocalDateTime.now(ZoneOffset.UTC));
                    }
                }
            }
        }
    }
}
