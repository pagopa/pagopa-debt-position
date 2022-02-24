package it.gov.pagopa.payments.config;

import it.gov.pagopa.payments.entity.Debitor;
import it.gov.pagopa.payments.entity.PaymentPosition;
import it.gov.pagopa.payments.mapper.ConvertDebitorModelToDebitor;
import it.gov.pagopa.payments.mapper.ConvertPaymentPositionDebitorToCsvPositionModel;
import it.gov.pagopa.payments.mapper.ConvertPaymentPositionToPaymentMinimalModel;
import it.gov.pagopa.payments.mapper.ConvertPaymentPositionToPaymentPositionDetailModel;
import it.gov.pagopa.payments.mapper.ConvertUploadCsvModelToPaymentsModel;
import org.modelmapper.Converter;
import org.modelmapper.ModelMapper;
import org.modelmapper.convention.MatchingStrategies;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import it.gov.pagopa.payments.model.CsvPositionModel;
import it.gov.pagopa.payments.model.DebitorModel;
import it.gov.pagopa.payments.model.PaymentMinimalModel;
import it.gov.pagopa.payments.model.PaymentPositionDetailModel;
import it.gov.pagopa.payments.model.PaymentsModel;
import it.gov.pagopa.payments.model.UploadCsvModel;

@Configuration
public class MappingsConfiguration {

    @Bean
    public ModelMapper modelMapper() {
	ModelMapper mapper = new ModelMapper();
	mapper.getConfiguration().setMatchingStrategy(MatchingStrategies.STRICT);

	Converter<DebitorModel, Debitor> convertDebitorModelToDebitor = new ConvertDebitorModelToDebitor();
	Converter<UploadCsvModel, PaymentsModel> converterUploadCsvModelToPaymentsModel = new ConvertUploadCsvModelToPaymentsModel();
	Converter<PaymentPosition, PaymentMinimalModel> convertPaymentPositionToPaymentMinimalModel = new ConvertPaymentPositionToPaymentMinimalModel();
	Converter<PaymentPosition, PaymentPositionDetailModel> convertPaymentPositionToPaymentPositionDetailModel = new ConvertPaymentPositionToPaymentPositionDetailModel();
	Converter<PaymentPosition, CsvPositionModel> convertPaymentPositionDebitorToCsvPositionModel = new ConvertPaymentPositionDebitorToCsvPositionModel();
	
	
	mapper.createTypeMap(DebitorModel.class, Debitor.class).setConverter(convertDebitorModelToDebitor);
	mapper.createTypeMap(UploadCsvModel.class, PaymentsModel.class).setConverter(converterUploadCsvModelToPaymentsModel);
	mapper.createTypeMap(PaymentPosition.class, PaymentMinimalModel.class).setConverter(convertPaymentPositionToPaymentMinimalModel);
	mapper.createTypeMap(PaymentPosition.class, PaymentPositionDetailModel.class).setConverter(convertPaymentPositionToPaymentPositionDetailModel);
	mapper.createTypeMap(PaymentPosition.class, CsvPositionModel.class).setConverter(convertPaymentPositionDebitorToCsvPositionModel);
	

	return mapper;
    }

}
