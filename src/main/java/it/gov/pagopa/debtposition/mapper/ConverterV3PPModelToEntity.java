package it.gov.pagopa.debtposition.mapper;

import it.gov.pagopa.debtposition.entity.Installment;
import it.gov.pagopa.debtposition.entity.PaymentOption;
import it.gov.pagopa.debtposition.entity.PaymentPosition;
import it.gov.pagopa.debtposition.entity.Transfer;
import it.gov.pagopa.debtposition.model.enumeration.OptionType;
import it.gov.pagopa.debtposition.model.pd.DebtorModel;
import it.gov.pagopa.debtposition.model.pd.Stamp;
import it.gov.pagopa.debtposition.model.pd.TransferModel;
import it.gov.pagopa.debtposition.model.v3.InstallmentModel;
import it.gov.pagopa.debtposition.model.v3.PaymentOptionModelV3;
import it.gov.pagopa.debtposition.model.v3.PaymentPositionModelV3;
import org.modelmapper.Converter;
import org.modelmapper.spi.MappingContext;

import java.time.LocalDateTime;
import java.util.*;
import java.util.stream.Collectors;

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
        destination.setCompanyName(source.getCompanyName());
        destination.setOfficeName(source.getOfficeName());
        destination.setValidityDate(getValidityDate(source.getPaymentOption()));

        mapAndUpdatePaymentOptions(source, destination);
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

    private void mapAndUpdatePaymentOptions(PaymentPositionModelV3 source, PaymentPosition destination) {
        Map<String, PaymentOption> managedOptionsByDebtorFiscalCode =
                destination.getPaymentOption().stream()
                        .collect(Collectors.toMap(PaymentOption::getDebtorFiscalCode, po -> po));

        List<PaymentOptionModelV3> sourceOptions = source.getPaymentOption();
        List<PaymentOption> optionsToRemove = new ArrayList<>(destination.getPaymentOption());

        // Covered cases:
        // - 1 Payment Option with [1:N] Installment (ie Opzione Rateale)
        // - [1:N] Payment Option with 1 Installment (ie Opzione Multipla)
        // - [1:N] Payment Option with 1 Installment and 1 Payment Option with [1:N] Installment (ie
        // Opzione Unica + Opzione Rateale)
        if (sourceOptions != null) {
            for (PaymentOptionModelV3 sourceOption : sourceOptions) {
                PaymentOption managedOpt = managedOptionsByDebtorFiscalCode.get(sourceOption.getDebtor().getFiscalCode());
                if (managedOpt != null) {
                    // UPDATE: the option
                    mapAndUpdateSinglePaymentOption(sourceOption, managedOpt);
                    optionsToRemove.remove(managedOpt);
                } else {
                    // CREATE: the option
                    PaymentOption po = PaymentOption.builder().build();
                    mapAndUpdateSinglePaymentOption(sourceOption, po);
                    destination.getPaymentOption().add(po);
                }
            }
        }
        // DELETE: remove the orphans options
        destination.getPaymentOption().removeAll(optionsToRemove);
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

    /**
     * @param source      the input model
     * @param destination the output entity
     */
    private void mapAndUpdateSinglePaymentOption(
            PaymentOptionModelV3 source, PaymentOption destination) {
        DebtorModel debtor = source.getDebtor();

        if (debtor != null) {
            destination.setDebtorCity(debtor.getCity());
            destination.setDebtorCivicNumber(debtor.getCivicNumber());
            destination.setDebtorCountry(debtor.getCountry());
            destination.setDebtorType(debtor.getType());
            destination.setDebtorEmail(debtor.getEmail());
            destination.setDebtorFiscalCode(debtor.getFiscalCode());
            destination.setDebtorFullName(debtor.getFullName());
            destination.setDebtorPhone(debtor.getPhone());
            destination.setDebtorPostalCode(debtor.getPostalCode());
            destination.setDebtorProvince(debtor.getProvince());
            destination.setDebtorRegion(debtor.getRegion());
            destination.setDebtorStreetName(debtor.getStreetName());
        }

        destination.setDescription(destination.getDescription());
        destination.setOptionType(source.getInstallments().size() > 1 ? OptionType.OPZIONE_RATEALE : OptionType.OPZIONE_UNICA);
        destination.setRetentionDate(source.getRetentionDate());

        mapAndUpdateInstallments(source, destination);
    }

    private void mapAndUpdateInstallments(
            PaymentOptionModelV3 source, PaymentOption destination) {
        Map<String, Installment> managedInstallmentsByIuv =
                destination.getInstallment().stream()
                        .collect(Collectors.toMap(Installment::getIuv, po -> po));

        List<InstallmentModel> sourceInstallments = source.getInstallments();
        List<Installment> installmentsToRemove = new ArrayList<>(destination.getInstallment());

        if (sourceInstallments != null) {
            for (InstallmentModel sourceInstallment : sourceInstallments) {
                Installment managedInstallment = managedInstallmentsByIuv.get(sourceInstallment.getIuv());
                if (managedInstallment != null) {
                    // UPDATE: the installment
                    mapAndUpdateSingleInstallment(sourceInstallment, managedInstallment);
                    installmentsToRemove.remove(managedInstallment);
                } else {
                    // CREATE: the installment
                    Installment inst = Installment.builder().build();
                    inst.setSendSync(false);
                    mapAndUpdateSingleInstallment(sourceInstallment, inst);
                    destination.getInstallment().add(inst);
                }
            }
        }

        // DELETE: remove the orphans installments
        destination.getInstallment().removeAll(installmentsToRemove);
    }

    /**
     * @param source      the input model
     * @param destination the output entity
     */
    private void mapAndUpdateSingleInstallment(
            InstallmentModel source, Installment destination) {
        destination.setNav(source.getNav());
        destination.setIuv(source.getIuv());
        destination.setAmount(source.getAmount());
        destination.setDescription(source.getDescription());
        destination.setDueDate(source.getDueDate());
        destination.setFee(source.getFee());
        destination.setLastUpdatedDate(LocalDateTime.now());

        mapAndUpdateTransfers(source, destination);
        // TODO METADATA mapAndUpdateOptionMetadata(source, destination);
    }

    private void mapAndUpdateTransfers(
            InstallmentModel sourceInstallment, Installment destination) {
        Map<String, Transfer> managedTransfersById =
                destination.getTransfer().stream()
                        .collect(Collectors.toMap(Transfer::getTransferId, t -> t));

        List<Transfer> transfersToRemove = new ArrayList<>(destination.getTransfer());

        if (sourceInstallment.getTransfer() != null) {
            for (TransferModel sourceTx : sourceInstallment.getTransfer()) {
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
        // TODO where has it gone  destination.setCompanyName(source.getCompanyName());
        destination.setIban(source.getIban());
        destination.setTransferId(source.getIdTransfer());
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

        //TODO mapAndUpdateTransferMetadata(source, destination);
    }

//    private void mapAndUpdateOptionMetadata(
//            InstallmentModel sourceInstallment, PaymentOption destination) {
//        Map<String, PaymentOptionMetadata> managedPaymentOptionMetadataByKey =
//                destination.getPaymentOptionMetadata().stream()
//                        .collect(Collectors.toMap(PaymentOptionMetadata::getKey, po -> po));
//
//        List<InstallmentMetadataModel> sourcePaymentOptionMetadata =
//                sourceInstallment.getInstallmentMetadata().stream().toList();
//        List<PaymentOptionMetadata> metadataToRemove =
//                new ArrayList<>(destination.getPaymentOptionMetadata());
//
//        if (sourcePaymentOptionMetadata != null) {
//            for (InstallmentMetadataModel sourceMetadata : sourcePaymentOptionMetadata) {
//                PaymentOptionMetadata managedMetadata =
//                        managedPaymentOptionMetadataByKey.get(sourceMetadata.getKey());
//
//                if (managedMetadata != null) {
//                    // UPDATE:
//                    sourceMetadata.setValue(managedMetadata.getValue());
//                    metadataToRemove.remove(managedMetadata);
//                } else {
//                    // CREATE:
//                    PaymentOptionMetadata md =
//                            PaymentOptionMetadata.builder()
//                                    .key(sourceMetadata.getKey())
//                                    .value(sourceMetadata.getValue())
//                                    .paymentOption(destination)
//                                    .build();
//                    destination.getPaymentOptionMetadata().add(md);
//                }
//            }
//        }
//        // DELETE:the orphans metadata are removed
//        destination.getPaymentOptionMetadata().removeAll(metadataToRemove);
//    }
//
//    private void mapAndUpdateTransferMetadata(TransferModel source, Transfer destination) {
//        Map<String, TransferMetadata> managedTransferMetadataByKey =
//                destination.getTransferMetadata().stream()
//                        .collect(Collectors.toMap(TransferMetadata::getKey, po -> po));
//
//        List<TransferMetadataModel> sourceTransferMetadata = source.getTransferMetadata();
//        List<TransferMetadata> metadataToRemove = new ArrayList<>(destination.getTransferMetadata());
//
//        if (sourceTransferMetadata != null) {
//            for (TransferMetadataModel sourceMetadata : sourceTransferMetadata) {
//                TransferMetadata managedMetadata =
//                        managedTransferMetadataByKey.get(sourceMetadata.getKey());
//
//                if (managedMetadata != null) {
//                    // UPDATE:
//                    sourceMetadata.setValue(managedMetadata.getValue());
//                    metadataToRemove.remove(managedMetadata);
//                } else {
//                    // CREATE:
//                    TransferMetadata md =
//                            TransferMetadata.builder()
//                                    .key(sourceMetadata.getKey())
//                                    .value(sourceMetadata.getValue())
//                                    .transfer(destination)
//                                    .build();
//                    destination.getTransferMetadata().add(md);
//                }
//            }
//        }
//        // DELETE:
//        destination.getTransferMetadata().removeAll(metadataToRemove);
//    }
}
