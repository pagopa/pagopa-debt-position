package it.gov.pagopa.payments.config;


import it.gov.pagopa.payments.entity.ReceiptEntity;
import it.gov.pagopa.payments.mapper.ConvertReceiptEntityToReceiptModelResponse;
import it.gov.pagopa.payments.model.ReceiptModelResponse;
import org.modelmapper.Converter;
import org.modelmapper.ModelMapper;
import org.modelmapper.convention.MatchingStrategies;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class MappingsConfiguration {

    @Bean
    public ModelMapper modelMapper() {
        ModelMapper mapper = new ModelMapper();
        mapper.getConfiguration().setMatchingStrategy(MatchingStrategies.STRICT);

        Converter<ReceiptEntity, ReceiptModelResponse> convertReceiptEntityToReceiptModelResponse = new ConvertReceiptEntityToReceiptModelResponse();

        mapper.createTypeMap(ReceiptEntity.class, ReceiptModelResponse.class).setConverter(convertReceiptEntityToReceiptModelResponse);

        return mapper;
    }

}
