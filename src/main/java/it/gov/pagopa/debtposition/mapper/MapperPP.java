package it.gov.pagopa.debtposition.mapper;

import it.gov.pagopa.debtposition.entity.*;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import lombok.experimental.UtilityClass;

@UtilityClass
public class MapperPP {

  /**
   * Aggiorna un'entità PaymentPosition gestita (destination) con i dati di un'entità in input
   * (source). Questo è il punto di ingresso per il mapping.
   */
  public void mapAndUpdatePaymentPosition(PaymentPosition source, PaymentPosition destination) {
    // --- 1. AGGIORNAMENTO CAMPI SCALARI (PADRE) ---
    destination.setIupd(source.getIupd());
    destination.setOrganizationFiscalCode(source.getOrganizationFiscalCode());
    destination.setPull(source.getPull());
    destination.setPayStandIn(source.getPayStandIn());
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
    destination.setServiceType(source.getServiceType());
    destination.setCompanyName(source.getCompanyName());
    destination.setOfficeName(source.getOfficeName());
    destination.setInsertedDate(source.getInsertedDate());
    destination.setPublishDate(source.getPublishDate());
    destination.setValidityDate(source.getValidityDate());
    destination.setMinDueDate(source.getMinDueDate());
    destination.setMaxDueDate(source.getMaxDueDate());
    destination.setStatus(source.getStatus());
    destination.setLastUpdatedDate(source.getLastUpdatedDate());
    destination.setPaymentDate(source.getPaymentDate());
    destination.setSwitchToExpired(source.getSwitchToExpired());

    // --- 2. SINCRONIZZAZIONE COLLEZIONE FIGLIA (PaymentOption) ---
    mapAndUpdatePaymentOptions(source, destination);
  }

  /**
   * Sincronizza la collezione di PaymentOption tra l'entità source e quella destination. Utilizza
   * una chiave di business (IUV) per abbinare gli elementi.
   */
  private void mapAndUpdatePaymentOptions(PaymentPosition source, PaymentPosition destination) {
    Map<String, PaymentOption> managedOptionsByIuv =
        destination.getPaymentOption().stream()
            .collect(Collectors.toMap(PaymentOption::getIuv, po -> po));

    List<PaymentOption> sourceOptions = source.getPaymentOption();
    List<PaymentOption> optionsToRemove = new ArrayList<>(destination.getPaymentOption());

    for (PaymentOption sourceOpt : sourceOptions) {
      PaymentOption managedOpt = managedOptionsByIuv.get(sourceOpt.getIuv());

      if (managedOpt != null) {
        // UPDATE: L'opzione esiste, la aggiorniamo ricorsivamente.
        mapAndUpdateSinglePaymentOption(sourceOpt, managedOpt);
        optionsToRemove.remove(managedOpt);
      } else {
        // CREATE: L'opzione è nuova. La aggiungiamo.
        destination.addPaymentOption(sourceOpt);
      }
    }

    // DELETE: Rimuoviamo le opzioni "orfane".
    destination.getPaymentOption().removeAll(optionsToRemove);
  }

  /** Aggiorna una singola istanza di PaymentOption. */
  private void mapAndUpdateSinglePaymentOption(PaymentOption source, PaymentOption destination) {
    // Aggiorniamo i campi scalari
    destination.setAmount(source.getAmount());
    destination.setCity(source.getCity());
    destination.setCivicNumber(source.getCivicNumber());
    destination.setCountry(source.getCountry());
    destination.setDebtorType(source.getDebtorType());
    destination.setDescription(source.getDescription());
    destination.setDueDate(source.getDueDate());
    destination.setEmail(source.getEmail());
    destination.setFee(source.getFee());
    destination.setFiscalCode(source.getFiscalCode());
    destination.setFullName(source.getFullName());
    destination.setIdFlowReporting(source.getIdFlowReporting());
    destination.setIdReceipt(source.getIdReceipt());
    destination.setInsertedDate(source.getInsertedDate());
    destination.setIsPartialPayment(source.getIsPartialPayment());
    destination.setIuv(source.getIuv());
    destination.setLastUpdatedDate(source.getLastUpdatedDate());
    destination.setLastUpdatedDateNotificationFee(source.getLastUpdatedDateNotificationFee());
    destination.setNav(source.getNav());
    destination.setNotificationFee(source.getNotificationFee());
    destination.setOrganizationFiscalCode(source.getOrganizationFiscalCode());
    destination.setPaymentDate(source.getPaymentDate());
    destination.setPaymentMethod(source.getPaymentMethod());
    destination.setPhone(source.getPhone());
    destination.setPostalCode(source.getPostalCode());
    destination.setProvince(source.getProvince());
    destination.setPspCode(source.getPspCode());
    destination.setPspCompany(source.getPspCompany());
    destination.setPspTaxCode(source.getPspTaxCode());
    destination.setRegion(source.getRegion());
    destination.setReportingDate(source.getReportingDate());
    destination.setRetentionDate(source.getRetentionDate());
    destination.setSendSync(source.getSendSync());
    destination.setStatus(source.getStatus());
    destination.setStreetName(source.getStreetName());

    // Sincronizzazione ricorsiva delle sotto-collezioni
    mapAndUpdateTransfers(source, destination);
    mapAndUpdateOptionMetadata(source, destination); // <-- CHIAMATA ALLA NUOVA LOGICA
  }

  /** Sincronizza la collezione di Transfer. */
  private void mapAndUpdateTransfers(PaymentOption source, PaymentOption destination) {
    // Assumiamo che Transfer abbia una chiave di business, es. "transferId"
    Map<String, Transfer> managedTransfersById =
        destination.getTransfer().stream()
            .collect(Collectors.toMap(Transfer::getIdTransfer, t -> t));

    List<Transfer> sourceTransfers = source.getTransfer();
    List<Transfer> transfersToRemove = new ArrayList<>(destination.getTransfer());

    for (Transfer sourceTx : sourceTransfers) {
      Transfer managedTx = managedTransfersById.get(sourceTx.getIdTransfer());
      if (managedTx != null) {
        // UPDATE
        mapAndUpdateSingleTransfer(sourceTx, managedTx);
        transfersToRemove.remove(managedTx);
      } else {
        // CREATE
        destination.addTransfer(sourceTx); // Assumendo esista addTransfer in PaymentOption
      }
    }
    // DELETE
    destination.getTransfer().removeAll(transfersToRemove);
  }

  /** Aggiorna una singola istanza di Transfer. */
  private void mapAndUpdateSingleTransfer(Transfer source, Transfer destination) {
    destination.setAmount(source.getAmount());
    destination.setCategory(source.getCategory());
    destination.setCompanyName(source.getCompanyName());
    destination.setHashDocument(source.getHashDocument());
    destination.setIban(source.getIban());
    destination.setIdTransfer(source.getIdTransfer());
    destination.setInsertedDate(source.getInsertedDate());
    destination.setIuv(source.getIuv());
    destination.setLastUpdatedDate(source.getLastUpdatedDate());
    destination.setOrganizationFiscalCode(source.getOrganizationFiscalCode());
    destination.setPostalIban(source.getPostalIban());
    destination.setProvincialResidence(source.getProvincialResidence());
    destination.setRemittanceInformation(source.getRemittanceInformation());
    destination.setStampType(source.getStampType());
    destination.setStatus(source.getStatus());

    // Sincronizzazione metadati del transfer
    mapAndUpdateTransferMetadata(source, destination); // <-- CHIAMATA ALLA NUOVA LOGICA
  }

  /**
   * Sincronizza i metadati di PaymentOption usando la strategia "clear and add". È efficiente per
   * collezioni di valori semplici senza una chiave di business stabile.
   */
  private void mapAndUpdateOptionMetadata(PaymentOption source, PaymentOption destination) {
    Map<String, PaymentOptionMetadata> managedPaymentOptionMetadataByKey =
        destination.getPaymentOptionMetadata().stream()
            .collect(Collectors.toMap(PaymentOptionMetadata::getKey, po -> po));

    List<PaymentOptionMetadata> sourcePaymentOptionMetadata = source.getPaymentOptionMetadata();
    List<PaymentOptionMetadata> metadataToRemove =
        new ArrayList<>(destination.getPaymentOptionMetadata());

    for (PaymentOptionMetadata sourceMetadata : sourcePaymentOptionMetadata) {
      PaymentOptionMetadata managedMetadata =
          managedPaymentOptionMetadataByKey.get(sourceMetadata.getKey());

      if (managedMetadata != null) {
        // UPDATE: L'opzione esiste, la aggiorniamo ricorsivamente.
        sourceMetadata.setValue(managedMetadata.getValue());
        metadataToRemove.remove(managedMetadata);
      } else {
        // CREATE: L'opzione è nuova. La aggiungiamo.
        destination.addPaymentOptionMetadata(sourceMetadata);
      }
    }

    // DELETE: Rimuoviamo le opzioni "orfane".
    destination.getPaymentOptionMetadata().removeAll(metadataToRemove);
  }

  /** Sincronizza i metadati di Transfer usando la strategia "clear and add". */
  private void mapAndUpdateTransferMetadata(Transfer source, Transfer destination) {
    Map<String, TransferMetadata> managedTransferMetadataByKey =
        destination.getTransferMetadata().stream()
            .collect(Collectors.toMap(TransferMetadata::getKey, po -> po));

    List<TransferMetadata> sourceTransferMetadata = source.getTransferMetadata();
    List<TransferMetadata> metadataToRemove = new ArrayList<>(destination.getTransferMetadata());

    for (TransferMetadata sourceMetadata : sourceTransferMetadata) {
      TransferMetadata managedMetadata = managedTransferMetadataByKey.get(sourceMetadata.getKey());

      if (managedMetadata != null) {
        // UPDATE: L'opzione esiste, la aggiorniamo ricorsivamente.
        sourceMetadata.setValue(managedMetadata.getValue());
        metadataToRemove.remove(managedMetadata);
      } else {
        // CREATE: L'opzione è nuova. La aggiungiamo.
        destination.addTransferMetadata(sourceMetadata);
      }
    }

    // DELETE: Rimuoviamo le opzioni "orfane".
    destination.getTransferMetadata().removeAll(metadataToRemove);
  }
}
