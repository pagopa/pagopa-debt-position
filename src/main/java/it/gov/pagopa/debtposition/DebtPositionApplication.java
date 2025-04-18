package it.gov.pagopa.debtposition;

import it.gov.pagopa.debtposition.entity.PaymentOption;
import it.gov.pagopa.debtposition.entity.PaymentPosition;
import it.gov.pagopa.debtposition.mapper.*;
import it.gov.pagopa.debtposition.model.payments.response.PaymentOptionWithDebtorInfoModelResponse;
import it.gov.pagopa.debtposition.model.pd.PaymentPositionModel;
import it.gov.pagopa.debtposition.model.v3.PaymentPositionModelV3;
import it.gov.pagopa.debtposition.model.v3.response.PaymentPositionModelResponseV3;
import org.modelmapper.Converter;
import org.modelmapper.ModelMapper;
import org.modelmapper.PropertyMap;
import org.modelmapper.convention.MatchingStrategies;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.cloud.openfeign.EnableFeignClients;
import org.springframework.context.annotation.Bean;
import org.springframework.retry.annotation.EnableRetry;

@SpringBootApplication
@EnableFeignClients
@EnableRetry
public class DebtPositionApplication {

  public static void main(String[] args) {
    SpringApplication.run(DebtPositionApplication.class, args);
  }

  @Bean
  public ModelMapper modelMapper() {
    ModelMapper modelMapper = new ModelMapper();
    modelMapper.getConfiguration().setMatchingStrategy(MatchingStrategies.STRICT);
    modelMapper.addMappings(
        new PropertyMap<PaymentPosition, PaymentPosition>() {
          @Override
          protected void configure() {
            skip(destination.getServiceType()); // Skip mapping of the serviceType field
          }
        });

    Converter<PaymentPositionModel, PaymentPosition> convertPPModelToPPEntity =
        new ConvertPPModelToPPEntityForUpdate();
    modelMapper
        .createTypeMap(PaymentPositionModel.class, PaymentPosition.class)
        .setConverter(convertPPModelToPPEntity);
    Converter<PaymentOption, PaymentOptionWithDebtorInfoModelResponse>
        convertPOEntityToPOWithDebtor = new ConvertPOEntityToPOWithDebtor();
    modelMapper
        .createTypeMap(PaymentOption.class, PaymentOptionWithDebtorInfoModelResponse.class)
        .setConverter(convertPOEntityToPOWithDebtor);
    // GPD version 3 (also known as OdP API) input mapper
    Converter<PaymentPositionModelV3, PaymentPosition> convertPPV3ModelToPPEntity =
        new ConverterV3PPModelToEntity();
    modelMapper
        .createTypeMap(PaymentPositionModelV3.class, PaymentPosition.class)
        .setConverter(convertPPV3ModelToPPEntity);
    // GPD version 3 (also known as OdP API) output mapper response
    Converter<PaymentPosition, PaymentPositionModelResponseV3> convertPPv3EntityToPPv3Response =
        new ConverterV3PPEntityToModelResponse();
    modelMapper
        .createTypeMap(PaymentPosition.class, PaymentPositionModelResponseV3.class)
        .setConverter(convertPPv3EntityToPPv3Response);
    // GPD version 3 (also known as OdP API) output mapper
    Converter<PaymentPosition, PaymentPositionModelV3> converterV3PPEntityToModel =
        new ConverterV3PPEntityToModel();
    modelMapper
        .createTypeMap(PaymentPosition.class, PaymentPositionModelV3.class)
        .setConverter(converterV3PPEntityToModel);

    return modelMapper;
  }
}
