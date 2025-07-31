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
    destination.setStatus(source.getStatus());
    destination.setPaymentDate(source.getPaymentDate());
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
        // UPDATE: L'opzione esiste, la aggiorniamo ricorsivamente.
        mapAndUpdateSinglePaymentOption(source, sourceOpt, managedOpt);
        optionsToRemove.remove(managedOpt);
      } else {
        // CREATE: L'opzione è nuova. La aggiungiamo.
        PaymentOption po = PaymentOption.builder().build();
        po.setSendSync(false);
        mapAndUpdateSinglePaymentOption(source, sourceOpt, po);
        destination.getPaymentOption().add(po);
      }
    }

    // DELETE: Rimuoviamo le opzioni "orfane".
    destination.getPaymentOption().removeAll(optionsToRemove);
  }

  /** Aggiorna una singola istanza di PaymentOption. */
  private void mapAndUpdateSinglePaymentOption(
      PaymentPositionModel paymentPosition, PaymentOptionModel source, PaymentOption destination) {
    // Aggiorniamo i campi scalari
    destination.setAmount(source.getAmount());
    destination.setCity(paymentPosition.getCity());
    destination.setCivicNumber(paymentPosition.getCivicNumber());
    destination.setCountry(paymentPosition.getCountry());
    destination.setDebtorType(paymentPosition.getType());
    destination.setDescription(source.getDescription());
    destination.setDueDate(source.getDueDate());
    destination.setEmail(paymentPosition.getEmail());
    destination.setFee(source.getFee());
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

    // Sincronizzazione ricorsiva delle sotto-collezioni
    mapAndUpdateTransfers(source, destination);
    mapAndUpdateOptionMetadata(source, destination); // <-- CHIAMATA ALLA NUOVA LOGICA
  }

  /** Sincronizza la collezione di Transfer. */
  private void mapAndUpdateTransfers(PaymentOptionModel source, PaymentOption destination) {
    // Assumiamo che Transfer abbia una chiave di business, es. "transferId"
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

  /** Aggiorna una singola istanza di Transfer. */
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

    // Sincronizzazione metadati del transfer
    mapAndUpdateTransferMetadata(source, destination); // <-- CHIAMATA ALLA NUOVA LOGICA
  }

  /**
   * Sincronizza i metadati di PaymentOption usando la strategia "clear and add". È efficiente per
   * collezioni di valori semplici senza una chiave di business stabile.
   */
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
        // UPDATE: L'opzione esiste, la aggiorniamo ricorsivamente.
        sourceMetadata.setValue(managedMetadata.getValue());
        metadataToRemove.remove(managedMetadata);
      } else {
        // CREATE: L'opzione è nuova. La aggiungiamo.
        PaymentOptionMetadata md =
            PaymentOptionMetadata.builder()
                .key(sourceMetadata.getKey())
                .value(sourceMetadata.getValue())
                .paymentOption(destination)
                .build();
        destination.getPaymentOptionMetadata().add(md);
      }
    }

    // DELETE: Rimuoviamo le opzioni "orfane".
    destination.getPaymentOptionMetadata().removeAll(metadataToRemove);
  }

  /** Sincronizza i metadati di Transfer usando la strategia "clear and add". */
  private void mapAndUpdateTransferMetadata(TransferModel source, Transfer destination) {
    Map<String, TransferMetadata> managedTransferMetadataByKey =
        destination.getTransferMetadata().stream()
            .collect(Collectors.toMap(TransferMetadata::getKey, po -> po));

    List<TransferMetadataModel> sourceTransferMetadata = source.getTransferMetadata();
    List<TransferMetadata> metadataToRemove = new ArrayList<>(destination.getTransferMetadata());

    for (TransferMetadataModel sourceMetadata : sourceTransferMetadata) {
      TransferMetadata managedMetadata = managedTransferMetadataByKey.get(sourceMetadata.getKey());

      if (managedMetadata != null) {
        // UPDATE: L'opzione esiste, la aggiorniamo ricorsivamente.
        sourceMetadata.setValue(managedMetadata.getValue());
        metadataToRemove.remove(managedMetadata);
      } else {
        // CREATE: L'opzione è nuova. La aggiungiamo.
        TransferMetadata md =
            TransferMetadata.builder()
                .key(sourceMetadata.getKey())
                .value(sourceMetadata.getValue())
                .transfer(destination)
                .build();
        destination.getTransferMetadata().add(md);
      }
    }

    // DELETE: Rimuoviamo le opzioni "orfane".
    destination.getTransferMetadata().removeAll(metadataToRemove);
  }
}
