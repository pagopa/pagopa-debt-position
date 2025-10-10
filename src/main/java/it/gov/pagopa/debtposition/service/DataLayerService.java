package it.gov.pagopa.debtposition.service;

import it.gov.pagopa.debtposition.entity.apd.PaymentOption;
import it.gov.pagopa.debtposition.entity.apd.PaymentPosition;
import it.gov.pagopa.debtposition.entity.odp.PaymentOptionOdp;
import it.gov.pagopa.debtposition.entity.odp.PaymentPositionOdp;
import it.gov.pagopa.debtposition.repository.apd.PaymentOptionRepository;
import it.gov.pagopa.debtposition.repository.apd.PaymentPositionRepository;
import it.gov.pagopa.debtposition.repository.odp.PaymentOptionOdpRepository;
import it.gov.pagopa.debtposition.repository.odp.PaymentPositionOdpRepository;
import lombok.extern.slf4j.Slf4j;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

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
    private final ModelMapper modelMapper;

    @Autowired
    public DataLayerService(PaymentPositionRepository paymentPositionRepository, PaymentPositionOdpRepository paymentPositionOdpRepository, PaymentOptionRepository paymentOptionRepository, PaymentOptionOdpRepository paymentOptionOdpRepository, ModelMapper modelMapper) {
        this.paymentPositionRepository = paymentPositionRepository;
        this.paymentPositionOdpRepository = paymentPositionOdpRepository;
        this.paymentOptionRepository = paymentOptionRepository;
        this.paymentOptionOdpRepository = paymentOptionOdpRepository;
        this.modelMapper = modelMapper;
    }

    @Transactional
    public PaymentPosition saveAndFlushPaymentPosition(PaymentPosition paymentPosition) {
        PaymentPosition response = paymentPosition;
        if (Boolean.TRUE.equals(writeOnNewSchema)) {
            PaymentPositionOdp paymentPositionOdp = modelMapper.map(paymentPosition, PaymentPositionOdp.class);
            paymentPositionOdpRepository.saveAndFlush(paymentPositionOdp);
        }
        if (Boolean.TRUE.equals(writeOnOldSchema) || !writeOnNewSchema) {
            response = paymentPositionRepository.saveAndFlush(paymentPosition);
        }

        return response;
    }

    @Transactional
    public PaymentPosition updateAndFlushPaymentPosition(PaymentPosition paymentPosition) {
        PaymentPosition response = paymentPosition;
        if (Boolean.TRUE.equals(writeOnNewSchema)) {
            PaymentPositionOdp paymentPositionOdp = paymentPositionOdpRepository.findByIupd(paymentPosition.getIupd());
            if(paymentPositionOdp != null){
                paymentPositionOdpRepository.delete(paymentPositionOdp);
                paymentPositionOdpRepository.flush();
            }

            PaymentPositionOdp updatedPaymentPositionOdp = modelMapper.map(paymentPosition, PaymentPositionOdp.class);
            paymentPositionOdpRepository.saveAndFlush(updatedPaymentPositionOdp);
        }
        if (Boolean.TRUE.equals(writeOnOldSchema) || !writeOnNewSchema) {
            response = paymentPositionRepository.saveAndFlush(paymentPosition);
        }

        return response;
    }

    @Transactional
    public PaymentOption savePaymentOption(PaymentOption paymentOption) {
        PaymentOptionOdp paymentOptionOdp = modelMapper.map(paymentOption, PaymentOptionOdp.class);
        paymentOptionOdpRepository.saveAndFlush(paymentOptionOdp);

        return paymentOptionRepository.saveAndFlush(paymentOption);
    }
}
