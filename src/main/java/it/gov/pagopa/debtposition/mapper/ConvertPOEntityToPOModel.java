package it.gov.pagopa.debtposition.mapper;

import it.gov.pagopa.debtposition.entity.PaymentOption;
import it.gov.pagopa.debtposition.model.pd.PaymentOptionMetadataModel;
import it.gov.pagopa.debtposition.model.pd.PaymentOptionModel;
import it.gov.pagopa.debtposition.model.pd.TransferModel;
import it.gov.pagopa.debtposition.util.ObjectMapperUtils;

import java.util.ArrayList;
import java.util.List;

public class ConvertPOEntityToPOModel {

  public static PaymentOptionModel convert(PaymentOption paymentOption) {
    PaymentOption source = paymentOption;
    PaymentOptionModel destination = new PaymentOptionModel();

    // PaymentOption info
    destination.setNav(source.getNav());
    destination.setIuv(source.getIuv());
    destination.setAmount(source.getAmount());
    destination.setDescription(source.getDescription());
    destination.setIsPartialPayment(source.getIsPartialPayment());
    destination.setDueDate(source.getDueDate());
    destination.setRetentionDate(source.getRetentionDate());
    destination.setFee(source.getFee());
    destination.setNotificationFee(source.getNotificationFee());
    
    destination.setPaymentOptionMetadata(ObjectMapperUtils.mapAll(source.getPaymentOptionMetadata(), PaymentOptionMetadataModel.class));

    List<TransferModel> list = new ArrayList<>();
    source.getTransfer().forEach(
            t -> list.add(ConvertTransferToTransferModel.convert(t)));
    destination.setTransfer(list);

    return destination;
  }
}
