package it.gov.pagopa.debtposition;

import org.modelmapper.Converter;
import org.modelmapper.ModelMapper;
import org.modelmapper.convention.MatchingStrategies;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.annotation.Bean;

import it.gov.pagopa.debtposition.controller.pd.mapper.ConvertPPModelToPPEntityForUpdate;
import it.gov.pagopa.debtposition.entity.PaymentPosition;
import it.gov.pagopa.debtposition.model.pd.PaymentPositionModel;

@SpringBootApplication
public class DebtPositionApplication {
    
    @Bean
    public ModelMapper modelMapper() {
        ModelMapper modelMapper = new ModelMapper();
        modelMapper.getConfiguration().setMatchingStrategy(MatchingStrategies.STRICT);
        
        Converter<PaymentPositionModel, PaymentPosition> convertPPModelToPPEntity = new ConvertPPModelToPPEntityForUpdate();
        
        modelMapper.createTypeMap(PaymentPositionModel.class, PaymentPosition.class).setConverter(convertPPModelToPPEntity);
        
        return modelMapper;
    }

    public static void main(String[] args) {
        SpringApplication.run(DebtPositionApplication.class, args);
    }

}
