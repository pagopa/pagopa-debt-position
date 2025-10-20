package it.gov.pagopa.debtposition.mapper;

import it.gov.pagopa.debtposition.entity.PaymentOption;
import it.gov.pagopa.debtposition.entity.PaymentPosition;
import it.gov.pagopa.debtposition.entity.Transfer;
import it.gov.pagopa.debtposition.mapper.utils.UtilityMapper;
import it.gov.pagopa.debtposition.model.enumeration.DebtPositionStatusV3;
import it.gov.pagopa.debtposition.model.enumeration.InstallmentStatus;
import it.gov.pagopa.debtposition.model.v3.InstallmentModel;
import it.gov.pagopa.debtposition.model.v3.PaymentOptionModelV3;
import it.gov.pagopa.debtposition.model.v3.PaymentPositionModelV3;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import org.modelmapper.Converter;
import org.modelmapper.spi.MappingContext;

public class ConverterV3PPEntityToModel
    implements Converter<PaymentPosition, PaymentPositionModelV3> {

  @Override
  public PaymentPositionModelV3 convert(
      MappingContext<PaymentPosition, PaymentPositionModelV3> context) {
    PaymentPosition source = context.getSource();
    PaymentPositionModelV3 destination = new PaymentPositionModelV3();

    destination.setIupd(source.getIupd());
    destination.setCompanyName(source.getCompanyName());
    destination.setOfficeName(source.getOfficeName());
    destination.setPaymentDate(source.getPaymentDate());
    destination.setStatus(DebtPositionStatusV3.valueOf(source.getStatus().name()));

    LocalDateTime validityDate = source.getValidityDate();

    List<PaymentOption> paymentOptions = source.getPaymentOption();
    if (paymentOptions == null || paymentOptions.isEmpty()) {
      return destination;
    }

    // Partitioning the payment options into partial and unique POs
    Map<Boolean, List<PaymentOption>> partitionedPO =
        paymentOptions.stream()
            .collect(Collectors.partitioningBy(PaymentOption::getIsPartialPayment));

    // Extracting the partial and unique POs
    List<PaymentOption> partialPO = partitionedPO.get(true);
    List<PaymentOption> uniquePO = partitionedPO.get(false);
    List<PaymentOptionModelV3> paymentOptionsToAdd = new ArrayList<>();

    if (null != partialPO && !partialPO.isEmpty()) {
      // If at least one of the partial POs is marked as switchToExpired, the whole PO must be
      boolean partialAnyMarkedExpired = partialPO.stream()
    	        .anyMatch(i -> Boolean.TRUE.equals(i.getSwitchToExpired()));
      PaymentOptionModelV3 pov3 = this.convertPartialPO(partialPO, validityDate, partialAnyMarkedExpired);
      paymentOptionsToAdd.add(pov3);
    }

    if (null != uniquePO && !uniquePO.isEmpty()) {
      List<PaymentOptionModelV3> pov3List = uniquePO.stream()
    	        .map(po -> convertUniquePO(po, validityDate, Boolean.TRUE.equals(po.getSwitchToExpired())))
    	        .toList();
      paymentOptionsToAdd.addAll(pov3List);
    }

    destination.setPaymentOption(paymentOptionsToAdd);

    return destination;
  }

  // 1 unique PO -> 1 PaymentOption composed by 1 installment
  private PaymentOptionModelV3 convertUniquePO(
      PaymentOption po, LocalDateTime validityDate, boolean switchToExpired) {
    PaymentOptionModelV3 pov3 = convert(po);
    pov3.setValidityDate(validityDate);
    pov3.setSwitchToExpired(switchToExpired);
    // set installment
    List<InstallmentModel> installments = Collections.singletonList(convertInstallment(po));
    pov3.setInstallments(installments);
    return pov3;
  }

  // N partial PO -> 1 PaymentOption composed by N installment
  private PaymentOptionModelV3 convertPartialPO(
      List<PaymentOption> partialPOs, LocalDateTime validityDate, boolean switchToExpired) {
    // Get only the first to fill common data for partial PO (retentionDate, insertedDate, debtor)
    PaymentOptionModelV3 pov3 = convert(partialPOs.get(0));
    pov3.setValidityDate(validityDate);
    pov3.setSwitchToExpired(switchToExpired);
    // Set installments
    List<InstallmentModel> installments =
        partialPOs.stream().map(this::convertInstallment).toList();
    pov3.setInstallments(installments);
    return pov3;
  }

  private PaymentOptionModelV3 convert(PaymentOption po) {
    PaymentOptionModelV3 pov3 = new PaymentOptionModelV3();
    pov3.setRetentionDate(po.getRetentionDate());
    pov3.setDebtor(UtilityMapper.extractDebtor(po));

    return pov3;
  }

  private InstallmentModel convertInstallment(PaymentOption po) {
    InstallmentModel inst = new InstallmentModel();

    inst.setNav(po.getNav());
    inst.setIuv(po.getIuv());
    inst.setAmount(po.getAmount());
    inst.setDescription(po.getDescription());
    inst.setDueDate(po.getDueDate());
    inst.setFee(po.getFee());
    inst.setNotificationFee(po.getNotificationFee());
    String poStatus = po.getStatus().name();
    if (poStatus.startsWith("PO_")) {
      inst.setStatus(InstallmentStatus.valueOf(poStatus.substring(3)));
    } else {
      // this should never be executed because all payment option states are prefixed with "PO_"
      inst.setStatus(InstallmentStatus.valueOf(poStatus));
    }
    // Set installment metadata
    inst.setInstallmentMetadata(UtilityMapper.convert(po.getPaymentOptionMetadata()));
    // Set transfers
    List<Transfer> transfers = po.getTransfer();
    inst.setTransfer(UtilityMapper.convertTransfers(transfers));

    return inst;
  }
}
