package it.gov.pagopa.debtposition.mapper;

import static it.gov.pagopa.debtposition.mapper.utils.UtilityMapper.UNDEFINED_DEBTOR;

import it.gov.pagopa.debtposition.entity.*;
import it.gov.pagopa.debtposition.model.enumeration.Type;
import it.gov.pagopa.debtposition.model.pd.DebtorModel;
import it.gov.pagopa.debtposition.model.pd.Stamp;
import it.gov.pagopa.debtposition.model.pd.TransferMetadataModel;
import it.gov.pagopa.debtposition.model.pd.TransferModel;
import it.gov.pagopa.debtposition.model.v3.InstallmentMetadataModel;
import it.gov.pagopa.debtposition.model.v3.InstallmentModel;
import it.gov.pagopa.debtposition.model.v3.PaymentOptionModelV3;
import it.gov.pagopa.debtposition.model.v3.PaymentPositionModelV3;
import java.time.LocalDateTime;
import java.util.*;
import java.util.stream.Collectors;
import org.modelmapper.Converter;
import org.modelmapper.spi.MappingContext;

public class ConverterV3PPModelToEntity
    implements Converter<PaymentPositionModelV3, PaymentPosition> {

  @Override
  public PaymentPosition convert(MappingContext<PaymentPositionModelV3, PaymentPosition> context) {
    PaymentPositionModelV3 source = context.getSource();
    PaymentPosition destination =
        context.getDestination() != null ? context.getDestination() : new PaymentPosition();

    mapAndUpdatePaymentPosition(source, destination);

    return destination;
  }

  public void mapAndUpdatePaymentPosition(
      PaymentPositionModelV3 source, PaymentPosition destination) {
    destination.setIupd(source.getIupd());
    destination.setPayStandIn(source.isPayStandIn());
    destination.setType(Type.F);
    destination.setFiscalCode(UNDEFINED_DEBTOR);
    destination.setFullName(UNDEFINED_DEBTOR);
    destination.setCompanyName(source.getCompanyName());
    destination.setOfficeName(source.getOfficeName());
    destination.setValidityDate(getValidityDate(source.getPaymentOption()));
    destination.setPaymentDate(source.getPaymentDate());
    destination.setSwitchToExpired(getSwitchToExpired(source.getPaymentOption()));

    mapAndUpdateInstallments(source, destination);
  }

  private LocalDateTime getValidityDate(List<PaymentOptionModelV3> paymentOptions) {

    if (paymentOptions == null) {
      return null;
    }

    LocalDateTime validityDate = null;
    // Find the minimum validityDate
    Optional<LocalDateTime> minValidityDate =
        paymentOptions.stream()
            .map(PaymentOptionModelV3::getValidityDate)
            .filter(Objects::nonNull) // Ensure we only deal with non-null values
            .min(Comparator.naturalOrder());

    if (minValidityDate.isPresent()) validityDate = minValidityDate.get();

    return validityDate;
  }

  private boolean getSwitchToExpired(List<PaymentOptionModelV3> paymentOptions) {
    if (paymentOptions == null) {
      return false;
    }
    // Check if any PaymentOptionModelV3 has switchToExpired as true
    // OR operation for the boolean field
    return paymentOptions.stream()
        .filter(Objects::nonNull)
        .anyMatch(PaymentOptionModelV3::getSwitchToExpired);
  }

  private void mapAndUpdateInstallments(
      PaymentPositionModelV3 source, PaymentPosition destination) {
    Map<String, PaymentOption> managedOptionsByIuv =
        destination.getPaymentOption().stream()
            .collect(Collectors.toMap(PaymentOption::getIuv, po -> po));

    List<PaymentOptionModelV3> sourceOptions = source.getPaymentOption();
    List<PaymentOption> optionsToRemove = new ArrayList<>(destination.getPaymentOption());

    // Covered cases:
    // - 1 Payment Option with [1:N] Installment (ie Opzione Rateale)
    // - [1:N] Payment Option with 1 Installment (ie Opzione Multipla)
    // - [1:N] Payment Option with 1 Installment and 1 Payment Option with [1:N] Installment (ie
    // Opzione Unica + Opzione Rateale)
    if (sourceOptions != null) {
      for (PaymentOptionModelV3 sourceOption : sourceOptions) {
        for (InstallmentModel installment : sourceOption.getInstallments()) {
          PaymentOption managedOpt = managedOptionsByIuv.get(installment.getIuv());
          if (managedOpt != null) {
            // UPDATE: the option
            mapAndUpdateSinglePaymentOption(sourceOption, installment, managedOpt);
            optionsToRemove.remove(managedOpt);
          } else {
            // CREATE: the option
            PaymentOption po = PaymentOption.builder().build();
            po.setSendSync(false);
            mapAndUpdateSinglePaymentOption(sourceOption, installment, po);
            destination.getPaymentOption().add(po);
          }
        }
      }
    }
    // DELETE: remove the orphans options
    destination.getPaymentOption().removeAll(optionsToRemove);
  }

  /**
   * @param source the input model
   * @param sourceInstallment the input installment
   * @param destination the output entity
   */
  private void mapAndUpdateSinglePaymentOption(
      PaymentOptionModelV3 source, InstallmentModel sourceInstallment, PaymentOption destination) {
    DebtorModel debtor = source.getDebtor();

    if (debtor != null) {
      destination.setCity(debtor.getCity());
      destination.setCivicNumber(debtor.getCivicNumber());
      destination.setCountry(debtor.getCountry());
      destination.setDebtorType(debtor.getType());
      destination.setEmail(debtor.getEmail());
      destination.setFiscalCode(debtor.getFiscalCode());
      destination.setFullName(debtor.getFullName());
      destination.setPhone(debtor.getPhone());
      destination.setPostalCode(debtor.getPostalCode());
      destination.setProvince(debtor.getProvince());
      destination.setRegion(debtor.getRegion());
      destination.setStreetName(debtor.getStreetName());
    }

    destination.setAmount(sourceInstallment.getAmount());
    destination.setDescription(sourceInstallment.getDescription());
    destination.setDueDate(sourceInstallment.getDueDate());
    destination.setFee(sourceInstallment.getFee());
    destination.setIsPartialPayment(source.getInstallments().size() > 1);
    destination.setIuv(sourceInstallment.getIuv());
    destination.setLastUpdatedDate(LocalDateTime.now());
    destination.setNav(sourceInstallment.getNav());
    destination.setRetentionDate(source.getRetentionDate());

    mapAndUpdateTransfers(sourceInstallment, destination);
    mapAndUpdateOptionMetadata(sourceInstallment, destination);
  }

  private void mapAndUpdateTransfers(
      InstallmentModel sourceInstallment, PaymentOption destination) {
    Map<String, Transfer> managedTransfersById =
        destination.getTransfer().stream()
            .collect(Collectors.toMap(Transfer::getIdTransfer, t -> t));

    List<TransferModel> sourceTransfers = sourceInstallment.getTransfer().stream().toList();
    List<Transfer> transfersToRemove = new ArrayList<>(destination.getTransfer());

    if (sourceTransfers != null) {
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

  private void mapAndUpdateOptionMetadata(
      InstallmentModel sourceInstallment, PaymentOption destination) {
    Map<String, PaymentOptionMetadata> managedPaymentOptionMetadataByKey =
        destination.getPaymentOptionMetadata().stream()
            .collect(Collectors.toMap(PaymentOptionMetadata::getKey, po -> po));

    List<InstallmentMetadataModel> sourcePaymentOptionMetadata =
        sourceInstallment.getInstallmentMetadata().stream().toList();
    List<PaymentOptionMetadata> metadataToRemove =
        new ArrayList<>(destination.getPaymentOptionMetadata());

    if (sourcePaymentOptionMetadata != null) {
      for (InstallmentMetadataModel sourceMetadata : sourcePaymentOptionMetadata) {
        PaymentOptionMetadata managedMetadata =
            managedPaymentOptionMetadataByKey.get(sourceMetadata.getKey());

        if (managedMetadata != null) {
          // UPDATE:
          sourceMetadata.setValue(managedMetadata.getValue());
          metadataToRemove.remove(managedMetadata);
        } else {
          // CREATE:
          PaymentOptionMetadata md =
              PaymentOptionMetadata.builder()
                  .key(sourceMetadata.getKey())
                  .value(sourceMetadata.getValue())
                  .paymentOption(destination)
                  .build();
          destination.getPaymentOptionMetadata().add(md);
        }
      }
    }
    // DELETE:the orphans metadata are removed
    destination.getPaymentOptionMetadata().removeAll(metadataToRemove);
  }

  private void mapAndUpdateTransferMetadata(TransferModel source, Transfer destination) {
    Map<String, TransferMetadata> managedTransferMetadataByKey =
        destination.getTransferMetadata().stream()
            .collect(Collectors.toMap(TransferMetadata::getKey, po -> po));

    List<TransferMetadataModel> sourceTransferMetadata = source.getTransferMetadata();
    List<TransferMetadata> metadataToRemove = new ArrayList<>(destination.getTransferMetadata());

    if (sourceTransferMetadata != null) {
      for (TransferMetadataModel sourceMetadata : sourceTransferMetadata) {
        TransferMetadata managedMetadata =
            managedTransferMetadataByKey.get(sourceMetadata.getKey());

        if (managedMetadata != null) {
          // UPDATE:
          sourceMetadata.setValue(managedMetadata.getValue());
          metadataToRemove.remove(managedMetadata);
        } else {
          // CREATE:
          TransferMetadata md =
              TransferMetadata.builder()
                  .key(sourceMetadata.getKey())
                  .value(sourceMetadata.getValue())
                  .transfer(destination)
                  .build();
          destination.getTransferMetadata().add(md);
        }
      }
    }
    // DELETE:
    destination.getTransferMetadata().removeAll(metadataToRemove);
  }
}
