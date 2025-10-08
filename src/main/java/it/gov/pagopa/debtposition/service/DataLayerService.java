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
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@Slf4j
public class DataLayerService {
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
    public PaymentPosition savePaymentPositionFromV1(PaymentPosition paymentPosition){
        PaymentPositionOdp paymentPositionOdp = modelMapper.map(paymentPosition, PaymentPositionOdp.class);
        paymentPositionOdpRepository.save(paymentPositionOdp);

        return paymentPositionRepository.save(paymentPosition);
    }

    @Transactional
    public PaymentPositionOdp savePaymentPositionFromV3(PaymentPositionOdp paymentPositionOdp){
        PaymentPosition paymentPosition = modelMapper.map(paymentPositionOdp, PaymentPosition.class);
        paymentPositionRepository.saveAndFlush(paymentPosition);

        return paymentPositionOdpRepository.saveAndFlush(paymentPositionOdp);
    }

    @Transactional
    public PaymentOption savePaymentOptionFromV1(PaymentOption paymentOption){
        PaymentOptionOdp paymentOptionOdp = modelMapper.map(paymentOption, PaymentOptionOdp.class);
        paymentOptionOdpRepository.saveAndFlush(paymentOptionOdp);

        return paymentOptionRepository.saveAndFlush(paymentOption);
    }

    @Transactional
    public PaymentOptionOdp savePaymentOptionFromV3(PaymentOptionOdp paymentOptionOdp){
        PaymentOption paymentOption = modelMapper.map(paymentOptionOdp, PaymentOption.class);
        paymentOptionRepository.saveAndFlush(paymentOption);

        return paymentOptionOdpRepository.saveAndFlush(paymentOptionOdp);
    }
}
