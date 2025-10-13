package it.gov.pagopa.debtposition.service;

import it.gov.pagopa.debtposition.entity.apd.PaymentOption;
import it.gov.pagopa.debtposition.entity.apd.PaymentPosition;
import it.gov.pagopa.debtposition.entity.odp.PaymentOptionOdp;
import it.gov.pagopa.debtposition.entity.odp.PaymentPositionOdp;
import it.gov.pagopa.debtposition.repository.apd.PaymentOptionRepository;
import it.gov.pagopa.debtposition.repository.apd.PaymentPositionRepository;
import it.gov.pagopa.debtposition.repository.odp.PaymentOptionOdpRepository;
import it.gov.pagopa.debtposition.repository.odp.PaymentPositionOdpRepository;
import it.gov.pagopa.debtposition.util.ObjectMapperUtils;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

@Service
@Slf4j
public class DataLayerService {
    @Value("${datalayer.service.writeOnOldSchema}")
    private boolean writeOnOldSchema;
    @Value("${datalayer.service.writeOnNewSchema}")
    private boolean writeOnNewSchema;

    private enum Schemas {
        ODP, APD
    }

    @Value("${datalayer.service.readFromSchema}")
    private Schemas readFromSchema;
    private final PaymentPositionRepository paymentPositionRepository;
    private final PaymentPositionOdpRepository paymentPositionOdpRepository;
    private final PaymentOptionRepository paymentOptionRepository;
    private final PaymentOptionOdpRepository paymentOptionOdpRepository;

    @Autowired
    public DataLayerService(PaymentPositionRepository paymentPositionRepository, PaymentPositionOdpRepository paymentPositionOdpRepository, PaymentOptionRepository paymentOptionRepository, PaymentOptionOdpRepository paymentOptionOdpRepository) {
        this.paymentPositionRepository = paymentPositionRepository;
        this.paymentPositionOdpRepository = paymentPositionOdpRepository;
        this.paymentOptionRepository = paymentOptionRepository;
        this.paymentOptionOdpRepository = paymentOptionOdpRepository;
    }

    // Payment Positions
    @Transactional
    public PaymentPosition saveAndFlushPaymentPosition(PaymentPosition paymentPosition) {
        PaymentPosition response = paymentPosition;
        if (Boolean.TRUE.equals(writeOnNewSchema)) {
            PaymentPositionOdp paymentPositionOdp = paymentPositionOdpRepository.findByIupd(paymentPosition.getIupd());
            if (paymentPositionOdp != null) {
                paymentPositionOdpRepository.delete(paymentPositionOdp);
                paymentPositionOdpRepository.flush();
            }

            paymentPositionOdp = ObjectMapperUtils.map(paymentPosition, PaymentPositionOdp.class);
            paymentPositionOdpRepository.saveAndFlush(paymentPositionOdp);
        }
        if (Boolean.TRUE.equals(writeOnOldSchema) || !writeOnNewSchema) {
            response = paymentPositionRepository.saveAndFlush(paymentPosition);
        }

        return response;
    }

    @Transactional
    public List<PaymentPosition> saveAllAndFlushPaymentPosition(List<PaymentPosition> paymentPositionList) {
        List<PaymentPosition> response = paymentPositionList;
        if (Boolean.TRUE.equals(writeOnNewSchema)) {
            List<PaymentPositionOdp> paymentPositionOdpToDelete = paymentPositionOdpRepository.findAllByIupdIn(paymentPositionList.parallelStream().map(PaymentPosition::getIupd).toList());
            if (!paymentPositionOdpToDelete.isEmpty()) {
                paymentPositionOdpRepository.deleteAll(paymentPositionOdpToDelete);
                paymentPositionOdpRepository.flush();
            }

            List<PaymentPositionOdp> paymentPositionOdpToSave = ObjectMapperUtils.mapAll(paymentPositionList, PaymentPositionOdp.class);
            paymentPositionOdpRepository.saveAllAndFlush(paymentPositionOdpToSave);
        }
        if (Boolean.TRUE.equals(writeOnOldSchema) || !writeOnNewSchema) {
            response = paymentPositionRepository.saveAllAndFlush(paymentPositionList);
        }

        return response;
    }

    @Transactional
    public void deleteAndFlushPaymentPosition(PaymentPosition paymentPosition) {
        if (Boolean.TRUE.equals(writeOnNewSchema)) {
            PaymentPositionOdp paymentPositionOdp = paymentPositionOdpRepository.findByIupd(paymentPosition.getIupd());
            if (paymentPositionOdp != null) {
                paymentPositionOdpRepository.delete(paymentPositionOdp);
            }
        }
        if (Boolean.TRUE.equals(writeOnOldSchema) || !writeOnNewSchema) {
            paymentPositionRepository.delete(paymentPosition);
        }
    }

    @Transactional
    public void deleteAllAndFlushPaymentPosition(List<PaymentPosition> paymentPositionList, List<String> multipleIupds) {
        if (Boolean.TRUE.equals(writeOnNewSchema)) {
            List<PaymentPositionOdp> paymentPositionOdpList = paymentPositionOdpRepository.findAllByIupdIn(multipleIupds);
            if (paymentPositionOdpList != null && !paymentPositionOdpList.isEmpty()) {
                paymentPositionOdpRepository.deleteAll(paymentPositionOdpList);
                paymentPositionOdpRepository.flush();
            }
        }
        if (Boolean.TRUE.equals(writeOnOldSchema) || !writeOnNewSchema) {
            paymentPositionRepository.deleteAll(paymentPositionList);
            paymentPositionRepository.flush();
        }
    }


    // Payment Options
    @Transactional
    public PaymentOption saveAndFlushPaymentOption(PaymentOption paymentOption) {
        PaymentOption response = paymentOption;
        if (Boolean.TRUE.equals(writeOnNewSchema)) {
            PaymentOptionOdp paymentOptionOdp = paymentOptionOdpRepository.findByInstallmentsIuv(paymentOption.getIuv());
            if (paymentOptionOdp != null) {
                paymentOptionOdpRepository.delete(paymentOptionOdp);
                paymentOptionOdpRepository.flush();
            }

            paymentOptionOdp = ObjectMapperUtils.map(paymentOption, PaymentOptionOdp.class);
            paymentOptionOdpRepository.saveAndFlush(paymentOptionOdp);
        }
        if (Boolean.TRUE.equals(writeOnOldSchema) || !writeOnNewSchema) {
            response = paymentOptionRepository.saveAndFlush(paymentOption);
        }

        return response;
    }
}
