package it.gov.pagopa.debtposition.util;

import it.gov.pagopa.debtposition.entity.Installment;
import it.gov.pagopa.debtposition.entity.PaymentPosition;
import it.gov.pagopa.debtposition.entity.Transfer;
import it.gov.pagopa.debtposition.mapper.*;
import it.gov.pagopa.debtposition.model.enumeration.InstallmentStatus;
import it.gov.pagopa.debtposition.model.enumeration.PaymentOptionStatus;
import it.gov.pagopa.debtposition.model.payments.response.PaidPaymentOptionModel;
import it.gov.pagopa.debtposition.model.payments.response.PaymentOptionModelResponse;
import it.gov.pagopa.debtposition.model.payments.response.PaymentOptionWithDebtorInfoModelResponse;
import it.gov.pagopa.debtposition.model.pd.PaymentPositionModel;
import it.gov.pagopa.debtposition.model.pd.Stamp;
import it.gov.pagopa.debtposition.model.pd.TransferModel;
import it.gov.pagopa.debtposition.model.pd.response.PaymentPositionModelBaseResponse;
import it.gov.pagopa.debtposition.model.pd.response.TransferModelResponse;
import it.gov.pagopa.debtposition.model.v3.PaymentPositionModelV3;
import it.gov.pagopa.debtposition.model.v3.response.PaymentPositionModelResponseV3;
import org.modelmapper.Converter;
import org.modelmapper.ModelMapper;
import org.modelmapper.PropertyMap;
import org.modelmapper.convention.MatchingStrategies;

import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;

public class ObjectMapperUtils {

    private static final ModelMapper modelMapper;

    private static final Converter<Transfer, Stamp> stampConverter =
            context -> {
                Transfer t = context.getSource();
                if (t.getHashDocument() == null
                        && t.getStampType() == null
                        && t.getProvincialResidence() == null) {
                    return null;
                }
                return new Stamp(t.getHashDocument(), t.getStampType(), t.getProvincialResidence());
            };

    /**
     * Model mapper property setting are specified in the following block. Default property matching
     * strategy is set to Strict see {@link MatchingStrategies} Custom mappings are added using {@link
     * ModelMapper#addMappings(PropertyMap)}
     */
    static {
        modelMapper = new ModelMapper();
        modelMapper.getConfiguration().setMatchingStrategy(MatchingStrategies.STRICT);

        // Convert PaymentPosition model to PaymentPosition entity
        Converter<PaymentPositionModel, PaymentPosition> convertPPModelToPPEntity =
                new ConvertPPModelToPPEntity();
        modelMapper
                .createTypeMap(PaymentPositionModel.class, PaymentPosition.class)
                .setConverter(convertPPModelToPPEntity);

        // Convert PaymentPosition entity to PaymentPositionModel
        Converter<PaymentPosition, PaymentPositionModel> convertPPEntityToPPModel =
                new ConvertPPEntityToModel();
        modelMapper
                .createTypeMap(PaymentPosition.class, PaymentPositionModel.class)
                .setConverter(convertPPEntityToPPModel);

        // Convert PaymentPosition entity to PaymentPosition model response
        Converter<PaymentPosition, PaymentPositionModelBaseResponse> converterPPEntityToModelResponse =
                new ConvertPPEntityToModelResponse();
        modelMapper
                .createTypeMap(PaymentPosition.class, PaymentPositionModelBaseResponse.class)
                .setConverter(converterPPEntityToModelResponse);

        // GPD version 1 converter used to return a Payment Option model response.
        Converter<Installment, PaymentOptionModelResponse>
                convertInstallmentEntityToPOModelResponse = new ConvertInstallmentEntityToPOModelResponse();
        modelMapper
                .createTypeMap(Installment.class, PaymentOptionModelResponse.class)
                .setConverter(convertInstallmentEntityToPOModelResponse);

        // GPD version 1 converter used to return a paid Payment Option data (differs from default
        // mapping by serviceType adding).
        Converter<Installment, PaidPaymentOptionModel> convertPOEntityToPaidPOModel =
                new ConvertInstallmentEntityToPaidPOModel();
        modelMapper
                .createTypeMap(Installment.class, PaidPaymentOptionModel.class)
                .setConverter(convertPOEntityToPaidPOModel);

        // GPD version 1 converter used to return a Payment Option with debtor data.
        Converter<Installment, PaymentOptionWithDebtorInfoModelResponse>
                convertInstallmentEntityToPOWithDebtor = new ConvertInstallmentEntityToPOWithDebtor();
        modelMapper
                .createTypeMap(Installment.class, PaymentOptionWithDebtorInfoModelResponse.class)
                .setConverter(convertInstallmentEntityToPOWithDebtor);

        // GPD version 3 (also known as OdP API) input mapper
        Converter<PaymentPositionModelV3, PaymentPosition> convertPPV3ModelToPPEntity =
                new ConvertPPModelV3ToEntity();
        modelMapper
                .createTypeMap(PaymentPositionModelV3.class, PaymentPosition.class)
                .setConverter(convertPPV3ModelToPPEntity);

        // GPD version 3 (also known as OdP API) output mapper response
        Converter<PaymentPosition, PaymentPositionModelResponseV3> convertPPv3EntityToPPv3Response =
                new ConvertPPEntityToModelResponseV3();
        modelMapper
                .createTypeMap(PaymentPosition.class, PaymentPositionModelResponseV3.class)
                .setConverter(convertPPv3EntityToPPv3Response);

        // GPD version 3 (also known as OdP API) output mapper
        Converter<PaymentPosition, PaymentPositionModelV3> converterV3PPEntityToModel =
                new ConvertPPEntityToModelV3();
        modelMapper
                .createTypeMap(PaymentPosition.class, PaymentPositionModelV3.class)
                .setConverter(converterV3PPEntityToModel);

        // Add custom mapping from Transfer to TransferModel
        modelMapper.addMappings(
                new PropertyMap<Transfer, TransferModel>() {
                    @Override
                    protected void configure() {
                        using(stampConverter).map(source, destination.getStamp());
                    }
                });

        // Add custom mapping from Transfer to TransferModelResponse
        modelMapper.addMappings(
                new PropertyMap<Transfer, TransferModelResponse>() {
                    @Override
                    protected void configure() {
                        using(stampConverter).map(source, destination.getStamp());
                    }
                });

        modelMapper.getConfiguration().setMatchingStrategy(MatchingStrategies.STRICT);
        modelMapper.addMappings(
                new PropertyMap<PaymentPosition, PaymentPosition>() {
                    @Override
                    protected void configure() {
                        skip(destination.getServiceType()); // Skip mapping of the serviceType field
                        skip(destination.getPaymentOption());
                    }
                });
    }

    public static PaymentOptionStatus mapInstallmentStatusToPoStatus(InstallmentStatus installmentStatus) {
        return switch (installmentStatus) {
            case UNPAID, EXPIRED, UNPAYABLE -> PaymentOptionStatus.PO_UNPAID;
            case PAID -> PaymentOptionStatus.PO_PAID;
            case PARTIALLY_REPORTED -> PaymentOptionStatus.PO_PARTIALLY_REPORTED;
            case REPORTED -> PaymentOptionStatus.PO_REPORTED;
        };
    }

    /**
     * Hide from public usage.
     */
    private ObjectMapperUtils() {
    }

    /**
     * Note: outClass object must have default constructor with no arguments
     *
     * @param <D>      type of result object.
     * @param <T>      type of source object to map from.
     * @param entity   entity that needs to be mapped.
     * @param outClass class of result object.
     * @return new object of <code>outClass</code> type.
     */
    public static <D, T> D map(final T entity, Class<D> outClass) {
        return modelMapper.map(entity, outClass);
    }

    /**
     * Note: outClass object must have default constructor with no arguments
     *
     * @param entityList list of entities that needs to be mapped
     * @param outCLass   class of result list element
     * @param <D>        type of objects in result list
     * @param <T>        type of entity in <code>entityList</code>
     * @return list of mapped object with <code><D></code> type.
     */
    public static <D, T> List<D> mapAll(final Collection<T> entityList, Class<D> outCLass) {
        return entityList.stream().map(entity -> map(entity, outCLass)).collect(Collectors.toList());
    }

    /**
     * Maps {@code source} to {@code destination}.
     *
     * @param source      object to map from
     * @param destination object to map to
     */
    public static <S, D> D map(final S source, D destination) {
        modelMapper.map(source, destination);
        return destination;
    }
}
