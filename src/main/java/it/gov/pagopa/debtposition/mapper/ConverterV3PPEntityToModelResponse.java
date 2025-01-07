package it.gov.pagopa.debtposition.mapper;

import it.gov.pagopa.debtposition.entity.*;
import it.gov.pagopa.debtposition.mapper.utils.UtilityMapper;
import it.gov.pagopa.debtposition.model.enumeration.DebtPositionStatusV3;
import it.gov.pagopa.debtposition.model.enumeration.InstallmentStatus;
import it.gov.pagopa.debtposition.model.v3.response.InstallmentModelResponse;
import it.gov.pagopa.debtposition.model.v3.response.PaymentOptionModelResponseV3;
import it.gov.pagopa.debtposition.model.v3.response.PaymentPositionModelResponseV3;
import java.time.LocalDateTime;
import java.util.Collections;
import java.util.List;
import java.util.Map;
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
    // todo check for status coherence
    destination.setStatus(DebtPositionStatusV3.valueOf(source.getStatus().name()));

    LocalDateTime validityDate = source.getValidityDate();
    Boolean switchToExpired = source.getSwitchToExpired();

    // Partitioning the payment options into partial and unique POs
    Map<Boolean, List<PaymentOption>> partitionedPO =
        source.getPaymentOption().stream()
            .collect(Collectors.partitioningBy(PaymentOption::getIsPartialPayment));

    // Extracting the partial and unique POs
    List<PaymentOption> partialPO = partitionedPO.get(true);
    List<PaymentOption> uniquePO = partitionedPO.get(false);

    if (!partialPO.isEmpty()) {
      PaymentOptionModelResponseV3 pov3 =
          this.convertPartialPO(partialPO, validityDate, switchToExpired);
      destination.addPaymentOption(pov3);
    }

    if (!uniquePO.isEmpty()) {
      List<PaymentOptionModelResponseV3> pov3List =
          uniquePO.stream().map(po -> convertUniquePO(po, validityDate, switchToExpired)).toList();
      destination.setPaymentOption(pov3List);
    }

    return destination;
  }

  // N partial PO -> 1 PaymentOption composed by N installment
  private PaymentOptionModelResponseV3 convertPartialPO(
      List<PaymentOption> partialPOs, LocalDateTime validityDate, boolean switchToExpired) {
    // Get only the first to fill common data for partial PO (retentionDate, insertedDate, debtor)
    PaymentOptionModelResponseV3 pov3 = convert(partialPOs.get(0));
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
      PaymentOption po, LocalDateTime validityDate, boolean switchToExpired) {
    PaymentOptionModelResponseV3 pov3 = convert(po);
    pov3.setValidityDate(validityDate);
    pov3.setSwitchToExpired(switchToExpired);
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
    inst.setStatus(InstallmentStatus.valueOf(po.getStatus().name().substring(3)));
    inst.setLastUpdatedDate(po.getLastUpdatedDate());
    // set installment metadata
    inst.setInstallmentMetadata(UtilityMapper.convert(po.getPaymentOptionMetadata()));
    // set transfers
    List<Transfer> transfers = po.getTransfer();
    inst.setTransfer(UtilityMapper.convertTransfersToResponse(transfers));

    return inst;
  }
}
