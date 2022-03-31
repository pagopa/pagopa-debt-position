package it.gov.pagopa.debtposition;

import it.gov.pagopa.debtposition.controller.pd.mapper.ConvertPOEntityToPOWithDebtor;
import it.gov.pagopa.debtposition.controller.pd.mapper.ConvertPPModelToPPEntityForUpdate;
import it.gov.pagopa.debtposition.entity.PaymentOption;
import it.gov.pagopa.debtposition.entity.PaymentPosition;
import it.gov.pagopa.debtposition.model.payments.response.PaymentOptionWithDebtorInfoModelResponse;
import it.gov.pagopa.debtposition.model.pd.PaymentPositionModel;
import org.modelmapper.Converter;
import org.modelmapper.ModelMapper;
import org.modelmapper.convention.MatchingStrategies;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.annotation.Bean;

@SpringBootApplication
public class DebtPositionApplication {

    public static void main(String[] args) {
        SpringApplication.run(DebtPositionApplication.class, args);
    }

    @Bean
    public ModelMapper modelMapper() {
        ModelMapper modelMapper = new ModelMapper();
        modelMapper.getConfiguration().setMatchingStrategy(MatchingStrategies.STRICT);

        Converter<PaymentPositionModel, PaymentPosition> convertPPModelToPPEntity = new ConvertPPModelToPPEntityForUpdate();
        Converter<PaymentOption, PaymentOptionWithDebtorInfoModelResponse> convertPOEntityToPOWithDebtor = new ConvertPOEntityToPOWithDebtor();
        modelMapper.createTypeMap(PaymentPositionModel.class, PaymentPosition.class).setConverter(convertPPModelToPPEntity);
        modelMapper.createTypeMap(PaymentOption.class, PaymentOptionWithDebtorInfoModelResponse.class).setConverter(convertPOEntityToPOWithDebtor);
        return modelMapper;
    }

}
