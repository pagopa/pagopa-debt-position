package it.gov.pagopa.debtposition.mapper;

import it.gov.pagopa.debtposition.entity.*;
import it.gov.pagopa.debtposition.mapper.utils.UtilityMapper;
import it.gov.pagopa.debtposition.model.enumeration.DebtPositionStatusV3;
import it.gov.pagopa.debtposition.model.enumeration.InstallmentStatus;
import it.gov.pagopa.debtposition.model.v3.response.InstallmentModelResponse;
import it.gov.pagopa.debtposition.model.v3.response.PaymentOptionModelResponseV3;
import it.gov.pagopa.debtposition.model.v3.response.PaymentPositionModelResponseV3;
import java.util.Collections;
import java.util.List;
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

    // Partial payment PO
    List<PaymentOption> partialPO =
        source.getPaymentOption().stream()
            .filter(po -> Boolean.TRUE.equals(po.getIsPartialPayment()))
            .toList();
    if (!partialPO.isEmpty()) {
      // get only the first to fill common data for partial PO (retentionDate, insertedDate, debtor)
      PaymentOptionModelResponseV3 pov3 = convert(partialPO.get(0));
      pov3.setValidityDate(source.getValidityDate());
      pov3.setSwitchToExpired(source.getSwitchToExpired());
      // set installments
      List<InstallmentModelResponse> installments =
          partialPO.stream().map(this::convertInstallment).toList();
      pov3.setInstallments(installments);
      destination.addPaymentOption(pov3);
    }

    // Unique payment PO
    List<PaymentOption> uniquePO =
        source.getPaymentOption().stream()
            .filter(po -> Boolean.FALSE.equals(po.getIsPartialPayment()))
            .toList();
    if (!uniquePO.isEmpty()) {
      PaymentOptionModelResponseV3 pov3;
      for (PaymentOption po : uniquePO) {
        pov3 = convert(po);
        pov3.setValidityDate(source.getValidityDate());
        pov3.setSwitchToExpired(source.getSwitchToExpired());
        // set installment
        List<InstallmentModelResponse> installments =
            Collections.singletonList(convertInstallment(po));
        pov3.setInstallments(installments);
        destination.addPaymentOption(pov3);
      }
    }

    return destination;
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
