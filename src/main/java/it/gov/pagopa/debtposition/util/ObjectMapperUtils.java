package it.gov.pagopa.debtposition.util;

import it.gov.pagopa.debtposition.entity.apd.PaymentPosition;
import it.gov.pagopa.debtposition.entity.apd.Transfer;
import it.gov.pagopa.debtposition.mapper.ConverterV3PPEntityToModelResponse;
import it.gov.pagopa.debtposition.model.pd.Stamp;
import it.gov.pagopa.debtposition.model.pd.TransferModel;
import it.gov.pagopa.debtposition.model.pd.response.TransferModelResponse;
import it.gov.pagopa.debtposition.model.v3.response.PaymentPositionModelResponseV3;
import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;
import org.modelmapper.Converter;
import org.modelmapper.ModelMapper;
import org.modelmapper.PropertyMap;
import org.modelmapper.convention.MatchingStrategies;

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

    // GPD version 3 (also known as OdP API) output mapper
    Converter<PaymentPosition, PaymentPositionModelResponseV3> convertPPv3EntityToPPv3Response =
        new ConverterV3PPEntityToModelResponse();
    modelMapper
        .createTypeMap(PaymentPosition.class, PaymentPositionModelResponseV3.class)
        .setConverter(convertPPv3EntityToPPv3Response);
  }

  /** Hide from public usage. */
  private ObjectMapperUtils() {}

  /**
   * Note: outClass object must have default constructor with no arguments
   *
   * @param <D> type of result object.
   * @param <T> type of source object to map from.
   * @param entity entity that needs to be mapped.
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
   * @param outCLass class of result list element
   * @param <D> type of objects in result list
   * @param <T> type of entity in <code>entityList</code>
   * @return list of mapped object with <code><D></code> type.
   */
  public static <D, T> List<D> mapAll(final Collection<T> entityList, Class<D> outCLass) {
    return entityList.stream().map(entity -> map(entity, outCLass)).collect(Collectors.toList());
  }

  /**
   * Maps {@code source} to {@code destination}.
   *
   * @param source object to map from
   * @param destination object to map to
   */
  public static <S, D> D map(final S source, D destination) {
    modelMapper.map(source, destination);
    return destination;
  }
}
