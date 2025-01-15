package it.gov.pagopa.debtposition.mapper;

import static it.gov.pagopa.debtposition.mapper.utils.UtilityMapper.UNDEFINED_DEBTOR;

import it.gov.pagopa.debtposition.entity.*;
import it.gov.pagopa.debtposition.exception.AppError;
import it.gov.pagopa.debtposition.exception.AppException;
import it.gov.pagopa.debtposition.mapper.utils.UtilityMapper;
import it.gov.pagopa.debtposition.model.enumeration.Type;
import it.gov.pagopa.debtposition.model.pd.*;
import it.gov.pagopa.debtposition.model.v3.InstallmentMetadataModel;
import it.gov.pagopa.debtposition.model.v3.InstallmentModel;
import it.gov.pagopa.debtposition.model.v3.PaymentOptionModelV3;
import it.gov.pagopa.debtposition.model.v3.PaymentPositionModelV3;
import java.time.LocalDateTime;
import java.util.Comparator;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import org.modelmapper.Converter;
import org.modelmapper.spi.MappingContext;
import org.springframework.util.CollectionUtils;

public class ConverterV3PPModelToEntity
    implements Converter<PaymentPositionModelV3, PaymentPosition> {

  @Override
  public PaymentPosition convert(MappingContext<PaymentPositionModelV3, PaymentPosition> context) {
    PaymentPositionModelV3 source = context.getSource();
    PaymentPosition destination =
        context.getDestination() != null ? context.getDestination() : new PaymentPosition();

    destination.setIupd(source.getIupd());
    destination.setCompanyName(source.getCompanyName());
    destination.setOfficeName(source.getOfficeName());
    destination.setPayStandIn(source.isPayStandIn());

    // debtor field in PaymentPosition
    destination.setType(Type.F);
    destination.setFiscalCode(UNDEFINED_DEBTOR);
    destination.setFullName(UNDEFINED_DEBTOR);
    // all remaining debtor fields are set to null

    List<PaymentOptionModelV3> paymentOpts = source.getPaymentOption();
    if (null == paymentOpts || paymentOpts.isEmpty()) {
      return destination;
    }

    destination.setValidityDate(getValidityDate(paymentOpts));
    destination.setSwitchToExpired(getSwitchToExpired(paymentOpts));

    // Check installment distribution in payment options by filtering for those with more than 1
    // installment
    long count =
        paymentOpts.stream()
            .filter(po -> po.getInstallments() != null && po.getInstallments().size() > 1)
            .count();

    // N payment options with N Installment is not possible (ie Opzione Rateale Multipla) ->
    // BAD_REQUEST
    if (count > 1)
      throw new AppException(
          AppError.DEBT_POSITION_REQUEST_DATA_ERROR,
          "Bad Request",
          "Multiple Installment plan not available");

    // Covered cases:
    // - 1 Payment Option with [1:N] Installment (ie Opzione Rateale)
    // - [1:N] Payment Option with 1 Installment (ie Opzione Multipla)
    // - [1:N] Payment Option with 1 Installment and 1 Payment Option with [1:N] Installment (ie
    // Opzione Unica + Opzione Rateale)
    for (PaymentOptionModelV3 pov3 : paymentOpts) {
      List<InstallmentModel> installments = pov3.getInstallments();
      if (installments != null) {
        int instCount = installments.size();
        boolean isPartialPayment = instCount > 1;
        installments.forEach(
            inst -> {
              PaymentOption po = this.convert(inst, pov3.getDebtor());
              po.setIsPartialPayment(isPartialPayment);
              po.setDueDate(inst.getDueDate());
              po.setRetentionDate(pov3.getRetentionDate());
              destination.addPaymentOption(po);
            });
      }
    }

    return destination;
  }

  private LocalDateTime getMaxDueDate(List<InstallmentModel> installments) {
    return installments.stream()
        .map(InstallmentModel::getDueDate)
        .max(Comparator.naturalOrder())
        // Returns null if no maxDueDate was found
        .orElse(null);
  }

  private LocalDateTime getValidityDate(List<PaymentOptionModelV3> paymentOptions) {
    LocalDateTime validityDate = null;
    // Find the minimum validityDate
    Optional<LocalDateTime> minValidityDate =
        paymentOptions.stream()
            .map(PaymentOptionModelV3::getValidityDate)
            .filter(Objects::nonNull) // Ensure we only deal with non-null values
            .min(Comparator.naturalOrder());

    if (minValidityDate.isPresent()) validityDate = minValidityDate.get();

    return validityDate;
  }

  private boolean getSwitchToExpired(List<PaymentOptionModelV3> paymentOptions) {
    // Check if any PaymentOptionModelV3 has switchToExpired as true
    // OR operation for the boolean field
    return paymentOptions.stream().anyMatch(PaymentOptionModelV3::getSwitchToExpired);
  }

  private PaymentOption convert(InstallmentModel inst, DebtorModel debtor) {
    PaymentOption po = new PaymentOption();

    po.setNav(inst.getNav());
    po.setIuv(inst.getIuv());
    po.setAmount(inst.getAmount());
    po.setDescription(inst.getDescription());
    po.setFee(inst.getFee());
    po.setNotificationFee(inst.getNotificationFee());
    // PO debtor fields
    po.setDebtorType(debtor.getType());
    po.setFiscalCode(debtor.getFiscalCode());
    po.setFullName(debtor.getFullName());
    po.setStreetName(debtor.getStreetName());
    po.setCivicNumber(debtor.getCivicNumber());
    po.setPostalCode(debtor.getPostalCode());
    po.setCity(debtor.getCity());
    po.setProvince(debtor.getProvince());
    po.setRegion(debtor.getRegion());
    po.setCountry(debtor.getCountry());
    po.setEmail(debtor.getEmail());
    po.setPhone(debtor.getPhone());

    List<TransferModel> transfers = inst.getTransfer();
    po.setTransfer(UtilityMapper.convertTransfersModel(transfers));

    List<InstallmentMetadataModel> metadata = inst.getInstallmentMetadata();
    if (!CollectionUtils.isEmpty(metadata)) {
      for (InstallmentMetadataModel m : metadata) {
        po.addPaymentOptionMetadata(
            PaymentOptionMetadata.builder().key(m.getKey()).value(m.getValue()).build());
      }
    }

    return po;
  }

  private boolean isPartialPayment(List<PaymentOptionModelV3> paymentOptions) {
    boolean isPartialPayment;

    // check installment distribution in payment options
    int count =
        (int)
            paymentOptions.stream()
                // Filter for those with more than 1 installment
                .filter(
                    option ->
                        option.getInstallments() != null && option.getInstallments().size() > 1)
                .count();
    // Count the number of elements, and check if there are at least 2
    switch (count) {
      case 0:
        // 1 Installment for each PaymentOption
        isPartialPayment = false;
        break;
      case 1:
        // N Installment for 1 PaymentOption
        if (paymentOptions.size() == 1) {
          isPartialPayment = true;
          break;
        } // else go to default
      default:
        // count > 1 -> BAD_REQUEST
        throw new AppException(
            AppError.DEBT_POSITION_REQUEST_DATA_ERROR,
            "Bad Request",
            "Multiple Installment plan not available");
        // break
    }

    return isPartialPayment;
  }
}
