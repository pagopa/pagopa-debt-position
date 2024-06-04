package it.gov.pagopa.debtposition.mapper;

import it.gov.pagopa.debtposition.entity.PaymentOption;
import it.gov.pagopa.debtposition.model.pd.response.PaymentOptionMetadataModelResponse;
import it.gov.pagopa.debtposition.model.pd.response.PaymentOptionModelResponse;
import it.gov.pagopa.debtposition.model.pd.response.TransferModelResponse;
import it.gov.pagopa.debtposition.util.ObjectMapperUtils;

import java.util.ArrayList;
import java.util.List;

public class ConvertPOEntityToPOResponse {

  public static PaymentOptionModelResponse convert(PaymentOption paymentOption) {
    PaymentOption source = paymentOption;
    PaymentOptionModelResponse destination = new PaymentOptionModelResponse();

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
    
    destination.setPaymentOptionMetadata(ObjectMapperUtils.mapAll(source.getPaymentOptionMetadata(), PaymentOptionMetadataModelResponse.class));

    List<TransferModelResponse> list = new ArrayList<>();
    source.getTransfer().forEach(
            t -> list.add(ConvertTransferToTransferResponse.convert(t)));
    destination.setTransfer(list);

    return destination;
  }
}
