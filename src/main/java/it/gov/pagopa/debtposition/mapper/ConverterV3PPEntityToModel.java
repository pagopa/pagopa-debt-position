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
import java.util.Collections;
import java.util.List;
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

    // Partial payment PO
    List<PaymentOption> partialPO =
        source.getPaymentOption().stream()
            .filter(po -> Boolean.TRUE.equals(po.getIsPartialPayment()))
            .toList();
    if (!partialPO.isEmpty()) {
      // get only the first to fill common data for partial PO (retentionDate, insertedDate, debtor)
      PaymentOptionModelV3 pov3 = convert(partialPO.get(0));
      pov3.setValidityDate(source.getValidityDate());
      pov3.setSwitchToExpired(source.getSwitchToExpired());
      // set installments
      List<InstallmentModel> installments =
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
      PaymentOptionModelV3 pov3;
      for (PaymentOption po : uniquePO) {
        pov3 = convert(po);
        pov3.setValidityDate(source.getValidityDate());
        pov3.setSwitchToExpired(source.getSwitchToExpired());
        // set installment
        List<InstallmentModel> installments = Collections.singletonList(convertInstallment(po));
        pov3.setInstallments(installments);
        destination.addPaymentOption(pov3);
      }
    }

    return destination;
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
    // substring to exclude prefix "PO_"
    inst.setStatus(InstallmentStatus.valueOf(po.getStatus().name().substring(3)));
    // set installment metadata
    inst.setInstallmentMetadata(UtilityMapper.convert(po.getPaymentOptionMetadata()));
    // set transfers
    List<Transfer> transfers = po.getTransfer();
    inst.setTransfer(UtilityMapper.convertTransfers(transfers));

    return inst;
  }
}
