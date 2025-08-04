package it.gov.pagopa.debtposition.mapper;

import it.gov.pagopa.debtposition.entity.*;
import it.gov.pagopa.debtposition.model.pd.*;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import javax.validation.constraints.NotNull;
import org.modelmapper.Converter;
import org.modelmapper.spi.MappingContext;

public class ConvertPPModelToPPEntity implements Converter<PaymentPositionModel, PaymentPosition> {

  @Override
  public PaymentPosition convert(
      MappingContext<@NotNull PaymentPositionModel, PaymentPosition> context) {
    PaymentPositionModel source = context.getSource();
    PaymentPosition destination =
        context.getDestination() != null ? context.getDestination() : new PaymentPosition();

    mapAndUpdatePaymentPosition(source, destination);

    return destination;
  }

  public void mapAndUpdatePaymentPosition(
      PaymentPositionModel source, PaymentPosition destination) {

    destination.setIupd(source.getIupd());
    destination.setPayStandIn(source.isPayStandIn());
    destination.setType(source.getType());
    destination.setFiscalCode(source.getFiscalCode());
    destination.setFullName(source.getFullName());
    destination.setStreetName(source.getStreetName());
    destination.setCivicNumber(source.getCivicNumber());
    destination.setPostalCode(source.getPostalCode());
    destination.setCity(source.getCity());
    destination.setProvince(source.getProvince());
    destination.setRegion(source.getRegion());
    destination.setCountry(source.getCountry());
    destination.setEmail(source.getEmail());
    destination.setPhone(source.getPhone());
    destination.setCompanyName(source.getCompanyName());
    destination.setOfficeName(source.getOfficeName());
    destination.setValidityDate(source.getValidityDate());
    destination.setSwitchToExpired(source.getSwitchToExpired());

    mapAndUpdatePaymentOptions(source, destination);
  }

  private void mapAndUpdatePaymentOptions(
      PaymentPositionModel source, PaymentPosition destination) {
    Map<String, PaymentOption> managedOptionsByIuv =
        destination.getPaymentOption().stream()
            .collect(Collectors.toMap(PaymentOption::getIuv, po -> po));

    List<PaymentOptionModel> sourceOptions = source.getPaymentOption();
    List<PaymentOption> optionsToRemove = new ArrayList<>(destination.getPaymentOption());

    for (PaymentOptionModel sourceOpt : sourceOptions) {
      PaymentOption managedOpt = managedOptionsByIuv.get(sourceOpt.getIuv());

      if (managedOpt != null) {
        // UPDATE
        mapAndUpdateSinglePaymentOption(source, sourceOpt, managedOpt);
        optionsToRemove.remove(managedOpt);
      } else {
        // CREATE
        PaymentOption po = PaymentOption.builder().build();
        po.setSendSync(false);
        mapAndUpdateSinglePaymentOption(source, sourceOpt, po);
        destination.getPaymentOption().add(po);
      }
    }

    // DELETE
    destination.getPaymentOption().removeAll(optionsToRemove);
  }

  private void mapAndUpdateSinglePaymentOption(
      PaymentPositionModel paymentPosition, PaymentOptionModel source, PaymentOption destination) {

    destination.setAmount(source.getAmount());
    destination.setCity(paymentPosition.getCity());
    destination.setCivicNumber(paymentPosition.getCivicNumber());
    destination.setCountry(paymentPosition.getCountry());
    destination.setDebtorType(paymentPosition.getType());
    destination.setDescription(source.getDescription());
    destination.setDueDate(source.getDueDate());
    destination.setEmail(paymentPosition.getEmail());
    destination.setFiscalCode(paymentPosition.getFiscalCode());
    destination.setFullName(paymentPosition.getFullName());
    destination.setIsPartialPayment(source.getIsPartialPayment());
    destination.setIuv(source.getIuv());
    destination.setLastUpdatedDate(LocalDateTime.now());
    destination.setNav(source.getNav());
    destination.setPhone(paymentPosition.getPhone());
    destination.setPostalCode(paymentPosition.getPostalCode());
    destination.setProvince(paymentPosition.getProvince());
    destination.setRegion(paymentPosition.getRegion());
    destination.setRetentionDate(source.getRetentionDate());
    destination.setStreetName(paymentPosition.getStreetName());

    mapAndUpdateTransfers(source, destination);
    mapAndUpdateOptionMetadata(source, destination);
  }

  private void mapAndUpdateTransfers(PaymentOptionModel source, PaymentOption destination) {
    Map<String, Transfer> managedTransfersById =
        destination.getTransfer().stream()
            .collect(Collectors.toMap(Transfer::getIdTransfer, t -> t));

    List<TransferModel> sourceTransfers = source.getTransfer();
    List<Transfer> transfersToRemove = new ArrayList<>(destination.getTransfer());

    for (TransferModel sourceTx : sourceTransfers) {
      Transfer managedTx = managedTransfersById.get(sourceTx.getIdTransfer());
      if (managedTx != null) {
        // UPDATE
        mapAndUpdateSingleTransfer(sourceTx, managedTx);
        transfersToRemove.remove(managedTx);
      } else {
        // CREATE
        Transfer tr = Transfer.builder().build();
        mapAndUpdateSingleTransfer(sourceTx, tr);
        destination.getTransfer().add(tr);
      }
    }
    // DELETE
    destination.getTransfer().removeAll(transfersToRemove);
  }

  private void mapAndUpdateSingleTransfer(TransferModel source, Transfer destination) {
    destination.setAmount(source.getAmount());
    destination.setCategory(source.getCategory());
    destination.setCompanyName(source.getCompanyName());
    destination.setIban(source.getIban());
    destination.setIdTransfer(source.getIdTransfer());
    destination.setLastUpdatedDate(LocalDateTime.now());
    destination.setOrganizationFiscalCode(source.getOrganizationFiscalCode());
    destination.setPostalIban(source.getPostalIban());
    destination.setRemittanceInformation(source.getRemittanceInformation());
    Stamp stamp = source.getStamp();
    if (stamp != null) {
      destination.setHashDocument(stamp.getHashDocument());
      destination.setProvincialResidence(stamp.getProvincialResidence());
      destination.setStampType(stamp.getStampType());
    }

    mapAndUpdateTransferMetadata(source, destination);
  }

  private void mapAndUpdateOptionMetadata(PaymentOptionModel source, PaymentOption destination) {
    Map<String, PaymentOptionMetadata> managedPaymentOptionMetadataByKey =
        destination.getPaymentOptionMetadata().stream()
            .collect(Collectors.toMap(PaymentOptionMetadata::getKey, po -> po));

    List<PaymentOptionMetadataModel> sourcePaymentOptionMetadata =
        source.getPaymentOptionMetadata();
    List<PaymentOptionMetadata> metadataToRemove =
        new ArrayList<>(destination.getPaymentOptionMetadata());

    for (PaymentOptionMetadataModel sourceMetadata : sourcePaymentOptionMetadata) {
      PaymentOptionMetadata managedMetadata =
          managedPaymentOptionMetadataByKey.get(sourceMetadata.getKey());

      if (managedMetadata != null) {
        // UPDATE
        sourceMetadata.setValue(managedMetadata.getValue());
        metadataToRemove.remove(managedMetadata);
      } else {
        // CREATE
        PaymentOptionMetadata md =
            PaymentOptionMetadata.builder()
                .key(sourceMetadata.getKey())
                .value(sourceMetadata.getValue())
                .paymentOption(destination)
                .build();
        destination.getPaymentOptionMetadata().add(md);
      }
    }

    // DELETE
    destination.getPaymentOptionMetadata().removeAll(metadataToRemove);
  }

  private void mapAndUpdateTransferMetadata(TransferModel source, Transfer destination) {
    Map<String, TransferMetadata> managedTransferMetadataByKey =
        destination.getTransferMetadata().stream()
            .collect(Collectors.toMap(TransferMetadata::getKey, po -> po));

    List<TransferMetadataModel> sourceTransferMetadata = source.getTransferMetadata();
    List<TransferMetadata> metadataToRemove = new ArrayList<>(destination.getTransferMetadata());

    for (TransferMetadataModel sourceMetadata : sourceTransferMetadata) {
      TransferMetadata managedMetadata = managedTransferMetadataByKey.get(sourceMetadata.getKey());

      if (managedMetadata != null) {
        // UPDATE
        sourceMetadata.setValue(managedMetadata.getValue());
        metadataToRemove.remove(managedMetadata);
      } else {
        // CREATE
        TransferMetadata md =
            TransferMetadata.builder()
                .key(sourceMetadata.getKey())
                .value(sourceMetadata.getValue())
                .transfer(destination)
                .build();
        destination.getTransferMetadata().add(md);
      }
    }

    // DELETE
    destination.getTransferMetadata().removeAll(metadataToRemove);
  }
}
