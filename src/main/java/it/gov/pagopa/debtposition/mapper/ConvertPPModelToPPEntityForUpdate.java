package it.gov.pagopa.debtposition.mapper;

import static it.gov.pagopa.debtposition.mapper.utils.UtilityMapper.UNDEFINED_DEBTOR;

import it.gov.pagopa.debtposition.entity.PaymentOption;
import it.gov.pagopa.debtposition.entity.PaymentOptionMetadata;
import it.gov.pagopa.debtposition.entity.PaymentPosition;
import it.gov.pagopa.debtposition.mapper.utils.UtilityMapper;
import it.gov.pagopa.debtposition.model.enumeration.Type;
import it.gov.pagopa.debtposition.model.pd.PaymentOptionMetadataModel;
import it.gov.pagopa.debtposition.model.pd.PaymentOptionModel;
import it.gov.pagopa.debtposition.model.pd.PaymentPositionModel;
import it.gov.pagopa.debtposition.model.pd.TransferModel;
import java.util.List;
import javax.validation.constraints.NotNull;
import org.modelmapper.Converter;
import org.modelmapper.spi.MappingContext;
import org.springframework.util.CollectionUtils;

public class ConvertPPModelToPPEntityForUpdate
    implements Converter<PaymentPositionModel, PaymentPosition> {

  @Override
  public PaymentPosition convert(
      MappingContext<@NotNull PaymentPositionModel, PaymentPosition> context) {
    PaymentPositionModel source = context.getSource();
    PaymentPosition destination =
        context.getDestination() != null ? context.getDestination() : new PaymentPosition();

    destination.setPayStandIn(source.isPayStandIn());
    destination.setCity(source.getCity());
    destination.setCivicNumber(source.getCivicNumber());
    destination.setCompanyName(source.getCompanyName());
    destination.setCountry(source.getCountry());
    destination.setEmail(source.getEmail());
    destination.setFiscalCode(source.getFiscalCode());
    destination.setFullName(source.getFullName());
    destination.setIupd(source.getIupd());
    destination.setOfficeName(source.getOfficeName());
    destination.setPhone(source.getPhone());
    destination.setPostalCode(source.getPostalCode());
    destination.setProvince(source.getProvince());
    destination.setRegion(source.getRegion());
    destination.setStreetName(source.getStreetName());
    destination.setType(source.getType());
    destination.setValidityDate(source.getValidityDate());
    destination.setSwitchToExpired(
        null != source.getSwitchToExpired() && source.getSwitchToExpired());

    List<PaymentOptionModel> paymentOpts = source.getPaymentOption();
    if (null != paymentOpts && !paymentOpts.isEmpty()) {
      for (PaymentOptionModel pOModel : paymentOpts) {
        destination.addPaymentOption(this.convertPOModelToPOEntityForUpdate(pOModel));
      }
    }

    return destination;
  }

  private PaymentOption convertPOModelToPOEntityForUpdate(PaymentOptionModel pom) {

    PaymentOption po = new PaymentOption();
    po.setAmount(pom.getAmount());
    po.setDescription(pom.getDescription());
    po.setDueDate(pom.getDueDate());
    po.setFee(pom.getFee());
    po.setIsPartialPayment(pom.getIsPartialPayment());
    po.setIuv(pom.getIuv());
    po.setNav(pom.getNav());
    po.setRetentionDate(pom.getRetentionDate());
    po.setNotificationFee(pom.getNotificationFee());
    // set debtor fields default values for v1 model
    po.setType(Type.F);
    po.setFullName(UNDEFINED_DEBTOR);
    po.setFiscalCode(UNDEFINED_DEBTOR);

    List<TransferModel> transfers = pom.getTransfer();
    po.setTransfer(UtilityMapper.convertTransfersModel(transfers));

    List<PaymentOptionMetadataModel> metadata = pom.getPaymentOptionMetadata();
    if (!CollectionUtils.isEmpty(metadata)) {
      for (PaymentOptionMetadataModel m : metadata) {
        po.addPaymentOptionMetadata(
            PaymentOptionMetadata.builder().key(m.getKey()).value(m.getValue()).build());
      }
    }

    return po;
  }
}
