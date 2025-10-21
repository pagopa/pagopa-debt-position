package it.gov.pagopa.debtposition.mapper;

import it.gov.pagopa.debtposition.entity.*;
import it.gov.pagopa.debtposition.mapper.utils.UtilityMapper;
import it.gov.pagopa.debtposition.model.enumeration.DebtPositionStatusV3;
import it.gov.pagopa.debtposition.model.enumeration.InstallmentStatus;
import it.gov.pagopa.debtposition.model.v3.response.InstallmentModelResponse;
import it.gov.pagopa.debtposition.model.v3.response.PaymentOptionModelResponseV3;
import it.gov.pagopa.debtposition.model.v3.response.PaymentPositionModelResponseV3;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;
import org.modelmapper.Converter;
import org.modelmapper.spi.MappingContext;

public class ConverterV3PPEntityToModelResponse
    implements Converter<PaymentPosition, PaymentPositionModelResponseV3> {

  @Override
  public PaymentPositionModelResponseV3 convert(
      MappingContext<PaymentPosition, PaymentPositionModelResponseV3> context) {
    PaymentPosition source = context.getSource();
    PaymentPositionModelResponseV3 destination = new PaymentPositionModelResponseV3();

    destination.setIupd(source.getIupd());
    destination.setOrganizationFiscalCode(source.getOrganizationFiscalCode());
    destination.setCompanyName(source.getCompanyName());
    destination.setOfficeName(source.getOfficeName());
    destination.setInsertedDate(source.getInsertedDate());
    destination.setPublishDate(source.getPublishDate());
    destination.setPaymentDate(source.getPaymentDate());
    destination.setLastUpdatedDate(source.getLastUpdatedDate());

    // check for status coherence and set
    String status = source.getStatus().name();
    String targetStatusV3 =
        switch (status) {
          case "EXPIRED", "INVALID" -> DebtPositionStatusV3.UNPAYABLE.toString();
          case "REPORTED" -> DebtPositionStatusV3.PAID.toString();
          default -> status;
        };
    destination.setStatus(DebtPositionStatusV3.valueOf(targetStatusV3));

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
    List<PaymentOptionModelResponseV3> paymentOptionsToAdd = new ArrayList<>();

    if (null != partialPO && !partialPO.isEmpty()) {
      // If at least one of the partial POs is marked as switchToExpired, the whole PO must be
      boolean partialAnyMarkedExpired = partialPO.stream()
  		      .anyMatch(i -> Boolean.TRUE.equals(i.getSwitchToExpired()));
      PaymentOptionModelResponseV3 pov3 =
          this.convertPartialPO(partialPO, partialAnyMarkedExpired);
      paymentOptionsToAdd.add(pov3);
    }

    if (null != uniquePO && !uniquePO.isEmpty()) {
    	List<PaymentOptionModelResponseV3> pov3List = uniquePO.stream()
    			.map(this::convertUniquePO)
    			.toList();
    	paymentOptionsToAdd.addAll(pov3List);
    }

    destination.setPaymentOption(paymentOptionsToAdd);

    return destination;
  }

  // N partial PO -> 1 PaymentOption composed by N installment
  private PaymentOptionModelResponseV3 convertPartialPO(
      List<PaymentOption> partialPOs, boolean switchToExpired) {
    // Get only the first to fill common data for partial PO (retentionDate, insertedDate, debtor)
    PaymentOptionModelResponseV3 pov3 = convert(partialPOs.get(0));
    // validityDate = min between the validity of the plan installments
    LocalDateTime validityDate = partialPOs.stream()
    	      .map(PaymentOption::getValidityDate)
    	      .filter(Objects::nonNull)
    	      .min(LocalDateTime::compareTo)
    	      .orElse(null);
    pov3.setValidityDate(validityDate);
    pov3.setSwitchToExpired(switchToExpired);
    // Set installments
    List<InstallmentModelResponse> installments =
        partialPOs.stream().map(this::convertInstallment).toList();
    pov3.setInstallments(installments);
    return pov3;
  }

  // 1 unique PO -> 1 PaymentOption composed by 1 installment
  private PaymentOptionModelResponseV3 convertUniquePO(
      PaymentOption po) {
    PaymentOptionModelResponseV3 pov3 = convert(po);
    pov3.setValidityDate(po.getValidityDate());
    pov3.setSwitchToExpired(Boolean.TRUE.equals(po.getSwitchToExpired()));
    // set installment
    List<InstallmentModelResponse> installments = Collections.singletonList(convertInstallment(po));
    pov3.setInstallments(installments);
    return pov3;
  }

  private PaymentOptionModelResponseV3 convert(PaymentOption po) {
    PaymentOptionModelResponseV3 pov3 = new PaymentOptionModelResponseV3();

    pov3.setRetentionDate(po.getRetentionDate());
    pov3.setInsertedDate(po.getInsertedDate());
    pov3.setDebtor(UtilityMapper.extractDebtor(po));

    return pov3;
  }

  private InstallmentModelResponse convertInstallment(PaymentOption po) {
    InstallmentModelResponse inst = new InstallmentModelResponse();

    inst.setNav(po.getNav());
    inst.setIuv(po.getIuv());
    inst.setOrganizationFiscalCode(po.getOrganizationFiscalCode());
    inst.setAmount(po.getAmount());
    inst.setDescription(po.getDescription());
    inst.setDueDate(po.getDueDate());
    inst.setPaymentDate(po.getPaymentDate());
    inst.setReportingDate(po.getReportingDate());
    inst.setPaymentMethod(po.getPaymentMethod());
    inst.setPspCompany(po.getPspCompany());
    inst.setFee(po.getFee());
    inst.setNotificationFee(po.getNotificationFee());
    inst.setIdReceipt(po.getIdReceipt());
    inst.setIdFlowReporting(po.getIdFlowReporting());
    // substring to exclude prefix "PO_"
    String poStatus = po.getStatus().name();
    if (poStatus.startsWith("PO_")) {
      inst.setStatus(InstallmentStatus.valueOf(poStatus.substring(3)));
    } else {
      // this branch should never be executed because all payment option states are prefixed with
      // "PO_"
      inst.setStatus(InstallmentStatus.valueOf(poStatus));
    }
    inst.setLastUpdatedDate(po.getLastUpdatedDate());
    // set installment metadata
    inst.setInstallmentMetadata(UtilityMapper.convert(po.getPaymentOptionMetadata()));
    // set transfers
    List<Transfer> transfers = po.getTransfer();
    inst.setTransfer(UtilityMapper.convertTransfersToResponse(transfers));

    return inst;
  }
}
