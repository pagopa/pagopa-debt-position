package it.gov.pagopa.debtposition.mapper;

import static it.gov.pagopa.debtposition.mapper.utils.UtilityMapper.UNDEFINED_DEBTOR;

import it.gov.pagopa.debtposition.entity.PaymentOption;
import it.gov.pagopa.debtposition.mapper.utils.UtilityMapper;
import it.gov.pagopa.debtposition.model.payments.response.PaymentOptionWithDebtorInfoModelResponse;
import it.gov.pagopa.debtposition.model.pd.response.PaymentOptionMetadataModelResponse;
import it.gov.pagopa.debtposition.util.ObjectMapperUtils;
import javax.validation.constraints.NotNull;
import org.modelmapper.Converter;
import org.modelmapper.spi.MappingContext;

public class ConvertPOEntityToPOWithDebtor
    implements Converter<PaymentOption, PaymentOptionWithDebtorInfoModelResponse> {

  @Override
  public PaymentOptionWithDebtorInfoModelResponse convert(
      MappingContext<@NotNull PaymentOption, PaymentOptionWithDebtorInfoModelResponse> context) {
    PaymentOption source = context.getSource();
    PaymentOptionWithDebtorInfoModelResponse destination =
        new PaymentOptionWithDebtorInfoModelResponse();

    // PaymentOption info
    destination.setNav(source.getNav());
    destination.setIuv(source.getIuv());
    destination.setOrganizationFiscalCode(source.getOrganizationFiscalCode());
    destination.setAmount(source.getAmount());
    destination.setDescription(source.getDescription());
    destination.setIsPartialPayment(source.getIsPartialPayment());
    destination.setDueDate(source.getDueDate());
    destination.setRetentionDate(source.getRetentionDate());
    destination.setPaymentDate(source.getPaymentDate());
    destination.setReportingDate(source.getReportingDate());
    destination.setPaymentMethod(source.getPaymentMethod());
    destination.setFee(source.getFee());
    destination.setNotificationFee(source.getNotificationFee());
    destination.setPspCompany(source.getPspCompany());
    destination.setIdReceipt(source.getIdReceipt());
    destination.setIdFlowReporting(source.getIdFlowReporting());
    destination.setStatus(source.getStatus());

    destination.setPaymentOptionMetadata(
        ObjectMapperUtils.mapAll(
            source.getPaymentOptionMetadata(), PaymentOptionMetadataModelResponse.class));

    destination.setDebtPositionStatus(source.getPaymentPosition().getStatus());

    // PaymentPosition creditor info
    destination.setIupd(source.getPaymentPosition().getIupd());
    destination.setCompanyName(source.getPaymentPosition().getCompanyName());
    destination.setOfficeName(source.getPaymentPosition().getOfficeName());
    destination.setPayStandIn(source.getPaymentPosition().getPayStandIn());
    destination.setServiceType(source.getPaymentPosition().getServiceType().name());

    // Get Debtor fields from Payment Position since is UNDEFINED on Payment Option
    String poFullName = source.getFullName();
    String poFiscalCode = source.getFiscalCode();
    if (null != poFullName
        && poFullName.equals(UNDEFINED_DEBTOR)
        && null != poFiscalCode
        && poFiscalCode.equals(UNDEFINED_DEBTOR)) {
      destination.setType(source.getPaymentPosition().getType());
      destination.setFullName(source.getPaymentPosition().getFullName());
      destination.setFiscalCode(source.getPaymentPosition().getFiscalCode());
      destination.setStreetName(source.getPaymentPosition().getStreetName());
      destination.setCivicNumber(source.getPaymentPosition().getCivicNumber());
      destination.setCity(source.getPaymentPosition().getCity());
      destination.setProvince(source.getPaymentPosition().getProvince());
      destination.setPostalCode(source.getPaymentPosition().getPostalCode());
      destination.setRegion(source.getPaymentPosition().getRegion());
      destination.setCountry(source.getPaymentPosition().getCountry());
      destination.setEmail(source.getPaymentPosition().getEmail());
      destination.setPhone(source.getPaymentPosition().getPhone());
    } else {
      destination.setType(source.getDebtorType());
      destination.setFullName(source.getFullName());
      destination.setFiscalCode(source.getFiscalCode());
      destination.setStreetName(source.getStreetName());
      destination.setCivicNumber(source.getCivicNumber());
      destination.setCity(source.getCity());
      destination.setProvince(source.getProvince());
      destination.setPostalCode(source.getPostalCode());
      destination.setRegion(source.getRegion());
      destination.setCountry(source.getCountry());
      destination.setEmail(source.getEmail());
      destination.setPhone(source.getPhone());
    }

    // transfers response
    destination.setTransfer(UtilityMapper.convertTransfersToResponse(source.getTransfer()));

    return destination;
  }
}
