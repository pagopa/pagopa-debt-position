package it.gov.pagopa.debtposition.mapper;

import static it.gov.pagopa.debtposition.mapper.utils.UtilityMapper.UNDEFINED_DEBTOR;

import it.gov.pagopa.debtposition.entity.*;
import it.gov.pagopa.debtposition.model.enumeration.Type;
import it.gov.pagopa.debtposition.model.pd.*;
import it.gov.pagopa.debtposition.model.v3.InstallmentMetadataModel;
import it.gov.pagopa.debtposition.model.v3.InstallmentModel;
import it.gov.pagopa.debtposition.model.v3.PaymentOptionModelV3;
import it.gov.pagopa.debtposition.model.v3.PaymentPositionModelV3;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import lombok.experimental.UtilityClass;

@UtilityClass
public class MapperPPV3 {

  public void mapAndUpdatePaymentPosition(
      PaymentPositionModelV3 source, PaymentPosition destination) {
    destination.setIupd(source.getIupd());
    destination.setPayStandIn(source.isPayStandIn());
    destination.setType(Type.F);
    destination.setFiscalCode(UNDEFINED_DEBTOR);
    destination.setFullName(UNDEFINED_DEBTOR);
    //        destination.setStreetName(source.getStreetName());
    //        destination.setCivicNumber(source.getCivicNumber());
    //        destination.setPostalCode(source.getPostalCode());
    //        destination.setCity(source.getCity());
    //        destination.setProvince(source.getProvince());
    //        destination.setRegion(source.getRegion());
    //        destination.setCountry(source.getCountry());
    //        destination.setEmail(source.getEmail());
    //        destination.setPhone(source.getPhone());
    destination.setCompanyName(source.getCompanyName());
    destination.setOfficeName(source.getOfficeName());
    //        destination.setValidityDate(source.getValidityDate());
    //        destination.setStatus(DebtPositionStatus.valueOf(source.getStatus().name()));
    destination.setPaymentDate(source.getPaymentDate());
    //        destination.setSwitchToExpired(source.getSwitchToExpired());

    mapAndUpdateInstallments(source, destination);
  }

  private void mapAndUpdateInstallments(
      PaymentPositionModelV3 source, PaymentPosition destination) {
    Map<String, PaymentOption> managedOptionsByIuv =
        destination.getPaymentOption().stream()
            .collect(Collectors.toMap(PaymentOption::getIuv, po -> po));

    List<PaymentOptionModelV3> sourceOptions = source.getPaymentOption();
    List<PaymentOption> optionsToRemove = new ArrayList<>(destination.getPaymentOption());

    for (PaymentOptionModelV3 sourceOption : sourceOptions) {
      for (InstallmentModel installment : sourceOption.getInstallments()) {
        PaymentOption managedOpt = managedOptionsByIuv.get(installment.getIuv());
        if (managedOpt != null) {
          // UPDATE: L'opzione esiste, la aggiorniamo ricorsivamente.
          mapAndUpdateSinglePaymentOption(sourceOption, installment, managedOpt);
          optionsToRemove.remove(managedOpt);
        } else {
          // CREATE: L'opzione è nuova. La aggiungiamo.
          PaymentOption po = PaymentOption.builder().build();
          po.setSendSync(false);
          mapAndUpdateSinglePaymentOption(sourceOption, installment, po);
          destination.getPaymentOption().add(po);
        }
      }
    }

    // DELETE: Rimuoviamo le opzioni "orfane".
    destination.getPaymentOption().removeAll(optionsToRemove);
  }

  /** Aggiorna una singola istanza di PaymentOption. */
  private void mapAndUpdateSinglePaymentOption(
      PaymentOptionModelV3 source, InstallmentModel sourceInstallment, PaymentOption destination) {
    DebtorModel debtor = source.getDebtor();

    // Aggiorniamo i campi scalari
    destination.setAmount(sourceInstallment.getAmount());
    destination.setCity(debtor.getCity());
    destination.setCivicNumber(debtor.getCivicNumber());
    destination.setCountry(debtor.getCountry());
    destination.setDebtorType(debtor.getType());
    destination.setDescription(source.getDescription());
    destination.setDueDate(sourceInstallment.getDueDate());
    destination.setEmail(debtor.getEmail());
    destination.setFee(sourceInstallment.getFee());
    destination.setFiscalCode(debtor.getFiscalCode());
    destination.setFullName(debtor.getFullName());
    destination.setIsPartialPayment(source.getInstallments().size() > 1);
    destination.setIuv(sourceInstallment.getIuv());
    destination.setLastUpdatedDate(LocalDateTime.now());
    destination.setNav(sourceInstallment.getNav());
    destination.setPhone(debtor.getPhone());
    destination.setPostalCode(debtor.getPostalCode());
    destination.setProvince(debtor.getProvince());
    destination.setRegion(debtor.getRegion());
    destination.setRetentionDate(source.getRetentionDate());
    destination.setStreetName(debtor.getStreetName());

    // Sincronizzazione ricorsiva delle sotto-collezioni
    mapAndUpdateTransfers(source, destination);
    mapAndUpdateOptionMetadata(source, destination); // <-- CHIAMATA ALLA NUOVA LOGICA
  }

  /** Sincronizza la collezione di Transfer. */
  private void mapAndUpdateTransfers(PaymentOptionModelV3 source, PaymentOption destination) {
    // Assumiamo che Transfer abbia una chiave di business, es. "transferId"
    Map<String, Transfer> managedTransfersById =
        destination.getTransfer().stream()
            .collect(Collectors.toMap(Transfer::getIdTransfer, t -> t));

    List<TransferModel> sourceTransfers =
        source.getInstallments().stream()
            .flatMap(installment -> installment.getTransfer().stream())
            .toList();
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
  private void mapAndUpdateOptionMetadata(PaymentOptionModelV3 source, PaymentOption destination) {
    Map<String, PaymentOptionMetadata> managedPaymentOptionMetadataByKey =
        destination.getPaymentOptionMetadata().stream()
            .collect(Collectors.toMap(PaymentOptionMetadata::getKey, po -> po));

    List<InstallmentMetadataModel> sourcePaymentOptionMetadata =
        source.getInstallments().stream()
            .flatMap(installment -> installment.getInstallmentMetadata().stream())
            .toList();
    List<PaymentOptionMetadata> metadataToRemove =
        new ArrayList<>(destination.getPaymentOptionMetadata());

    for (InstallmentMetadataModel sourceMetadata : sourcePaymentOptionMetadata) {
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
