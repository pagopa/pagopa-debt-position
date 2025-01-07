package it.gov.pagopa.debtposition.mapper.utils;

import it.gov.pagopa.debtposition.entity.PaymentOption;
import it.gov.pagopa.debtposition.entity.PaymentOptionMetadata;
import it.gov.pagopa.debtposition.entity.Transfer;
import it.gov.pagopa.debtposition.entity.TransferMetadata;
import it.gov.pagopa.debtposition.model.pd.DebtorModel;
import it.gov.pagopa.debtposition.model.pd.Stamp;
import it.gov.pagopa.debtposition.model.pd.TransferMetadataModel;
import it.gov.pagopa.debtposition.model.pd.TransferModel;
import it.gov.pagopa.debtposition.model.pd.response.TransferMetadataModelResponse;
import it.gov.pagopa.debtposition.model.pd.response.TransferModelResponse;
import it.gov.pagopa.debtposition.model.v3.InstallmentMetadataModel;
import it.gov.pagopa.debtposition.util.ObjectMapperUtils;
import java.util.ArrayList;
import java.util.List;
import org.springframework.util.CollectionUtils;

public class UtilityMapper {

  public static DebtorModel extractDebtor(PaymentOption po) {
    DebtorModel debtor = new DebtorModel();
    debtor.setType(po.getType());
    debtor.setFiscalCode(po.getFiscalCode());
    debtor.setFullName(po.getFullName());
    debtor.setStreetName(po.getStreetName());
    debtor.setCivicNumber(po.getCivicNumber());
    debtor.setPostalCode(po.getPostalCode());
    debtor.setCity(po.getCity());
    debtor.setProvince(po.getProvince());
    debtor.setRegion(po.getRegion());
    debtor.setCountry(po.getCountry());
    debtor.setEmail(po.getEmail());
    debtor.setPhone(po.getPhone());
    return debtor;
  }

  // Transfer entity, model, response converters

  public static List<TransferModel> convertTransfers(List<Transfer> transfers) {
    List<TransferModel> transfersModel = new ArrayList<>();
    if (null != transfers && !CollectionUtils.isEmpty(transfers)) {
      transfersModel = transfers.stream().map(UtilityMapper::convert).toList();
    }
    return transfersModel;
  }

  public static List<TransferModelResponse> convertTransfersToResponse(List<Transfer> transfers) {
    List<TransferModelResponse> transfersModel = new ArrayList<>();
    if (null != transfers && !CollectionUtils.isEmpty(transfers)) {
      transfersModel = transfers.stream().map(UtilityMapper::convertToResponse).toList();
    }
    return transfersModel;
  }

  public static List<Transfer> convertTransfersModel(List<TransferModel> transfersModel) {
    List<Transfer> transfers = new ArrayList<>();
    if (null != transfersModel && !CollectionUtils.isEmpty(transfersModel)) {
      transfers = transfersModel.stream().map(UtilityMapper::convert).toList();
    }
    return transfers;
  }

  // Payment option metadata to Installment metadata converter

  public static List<InstallmentMetadataModel> convert(List<PaymentOptionMetadata> poMetadata) {
    List<InstallmentMetadataModel> installmentsMetadata = new ArrayList<>();
    if (null != poMetadata && !CollectionUtils.isEmpty(poMetadata)) {
      installmentsMetadata =
          poMetadata.stream()
              .map(
                  m ->
                      InstallmentMetadataModel.builder()
                          .key(m.getKey())
                          .value(m.getValue())
                          .build())
              .toList();
    }
    return installmentsMetadata;
  }

  // Transfer metadata converters

  public static List<TransferMetadataModel> convertTransferMetadata(
      List<TransferMetadata> transferMetadata) {
    List<TransferMetadataModel> transferMetadataModels = new ArrayList<>();
    if (null != transferMetadata && !CollectionUtils.isEmpty(transferMetadata)) {
      transferMetadataModels =
          transferMetadata.stream()
              .map(m -> TransferMetadataModel.builder().key(m.getKey()).value(m.getValue()).build())
              .toList();
    }
    return transferMetadataModels;
  }

  public static List<TransferMetadataModelResponse> convertTransferMetadataResponse(
      List<TransferMetadata> transferMetadata) {
    List<TransferMetadataModelResponse> transferMetadataModelResponses = new ArrayList<>();
    if (null != transferMetadata && !CollectionUtils.isEmpty(transferMetadata)) {
      transferMetadataModelResponses =
          transferMetadata.stream()
              .map(
                  m ->
                      TransferMetadataModelResponse.builder()
                          .key(m.getKey())
                          .value(m.getValue())
                          .build())
              .toList();
    }
    return transferMetadataModelResponses;
  }

  // ##################################
  // #######  Private methods #########
  // ##################################
  private static TransferModel convert(Transfer t) {
    TransferModel destination = new TransferModel();

    destination.setOrganizationFiscalCode(t.getOrganizationFiscalCode());
    destination.setCompanyName(t.getCompanyName());
    destination.setIdTransfer(t.getIdTransfer());
    destination.setAmount(t.getAmount());
    destination.setRemittanceInformation(t.getRemittanceInformation());
    destination.setCategory(t.getCategory());
    destination.setIban(t.getIban());
    destination.setPostalIban(t.getPostalIban());

    // if one of Stamp attributes are different from null return Stamp values
    if (t.getHashDocument() != null
        || t.getStampType() != null
        || t.getProvincialResidence() != null) {
      destination.setStamp(
          Stamp.builder()
              .hashDocument(t.getHashDocument())
              .provincialResidence(t.getProvincialResidence())
              .stampType(t.getStampType())
              .build());
    }

    destination.setTransferMetadata(
        ObjectMapperUtils.mapAll(t.getTransferMetadata(), TransferMetadataModel.class));

    List<TransferMetadataModel> transferMetadataModelResponses =
        convertTransferMetadata(t.getTransferMetadata());
    destination.setTransferMetadata(transferMetadataModelResponses);

    return destination;
  }

  private static Transfer convert(TransferModel tm) {

    Transfer t = new Transfer();
    t.setAmount(tm.getAmount());
    t.setOrganizationFiscalCode(tm.getOrganizationFiscalCode());
    t.setCompanyName(tm.getCompanyName());
    t.setCategory(tm.getCategory());
    t.setIban(tm.getIban());
    t.setIdTransfer(tm.getIdTransfer());
    t.setPostalIban(tm.getPostalIban());
    if (tm.getStamp() != null) {
      t.setHashDocument(tm.getStamp().getHashDocument());
      t.setStampType(tm.getStamp().getStampType());
      t.setProvincialResidence(tm.getStamp().getProvincialResidence());
    }
    t.setRemittanceInformation(tm.getRemittanceInformation());

    List<TransferMetadataModel> metadata = tm.getTransferMetadata();
    if (!CollectionUtils.isEmpty(metadata)) {
      for (TransferMetadataModel m : metadata) {
        t.addTransferMetadata(
            TransferMetadata.builder().key(m.getKey()).value(m.getValue()).build());
      }
    }

    return t;
  }

  private static TransferModelResponse convertToResponse(Transfer t) {
    TransferModelResponse destination = new TransferModelResponse();

    destination.setOrganizationFiscalCode(t.getOrganizationFiscalCode());
    destination.setCompanyName(t.getCompanyName());
    destination.setIdTransfer(t.getIdTransfer());
    destination.setAmount(t.getAmount());
    destination.setRemittanceInformation(t.getRemittanceInformation());
    destination.setCategory(t.getCategory());
    destination.setIban(t.getIban());
    destination.setPostalIban(t.getPostalIban());
    destination.setInsertedDate(t.getInsertedDate());
    destination.setStatus(t.getStatus());
    destination.setLastUpdatedDate(t.getLastUpdatedDate());

    // if one of Stamp attributes are different from null return Stamp values
    if (t.getHashDocument() != null
        || t.getStampType() != null
        || t.getProvincialResidence() != null) {
      destination.setStamp(
          Stamp.builder()
              .hashDocument(t.getHashDocument())
              .provincialResidence(t.getProvincialResidence())
              .stampType(t.getStampType())
              .build());
    }

    List<TransferMetadataModelResponse> transferMetadataModelResponses =
        convertTransferMetadataResponse(t.getTransferMetadata());
    destination.setTransferMetadata(transferMetadataModelResponses);

    return destination;
  }
}
