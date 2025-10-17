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

        // TODO verify
        List<PaymentOptionModelV3> sourcePaymentOptionList = source.getPaymentOption();
        if(sourcePaymentOptionList != null && !sourcePaymentOptionList.isEmpty()){
            List<InstallmentModel> sourceInstallment = sourcePaymentOptionList.get(0).getInstallments();
            if(sourceInstallment != null && !sourceInstallment.isEmpty()){
                List<TransferModel> sourceTransferList = sourceInstallment.get(0).getTransfer();
                if(sourceTransferList != null && !sourceTransferList.isEmpty()){
                    destination.setOrganizationFiscalCode(sourceTransferList.get(0).getOrganizationFiscalCode());
                }
            }
        }

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
                    mapAndUpdateSinglePaymentOption(sourceOption, managedOpt, destination);
                    optionsToRemove.remove(managedOpt);
                } else {
                    // CREATE: the option
                    PaymentOption po = PaymentOption.builder().build();
                    mapAndUpdateSinglePaymentOption(sourceOption, po, destination);
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
     * @param destinationPo the output entity
     */
    private void mapAndUpdateSinglePaymentOption(
            PaymentOptionModelV3 source, PaymentOption destinationPo, PaymentPosition destination) {
        DebtorModel debtor = source.getDebtor();

        if (debtor != null) {
            destinationPo.setDebtorCity(debtor.getCity());
            destinationPo.setDebtorCivicNumber(debtor.getCivicNumber());
            destinationPo.setDebtorCountry(debtor.getCountry());
            destinationPo.setDebtorType(debtor.getType());
            destinationPo.setDebtorEmail(debtor.getEmail());
            destinationPo.setDebtorFiscalCode(debtor.getFiscalCode());
            destinationPo.setDebtorFullName(debtor.getFullName());
            destinationPo.setDebtorPhone(debtor.getPhone());
            destinationPo.setDebtorPostalCode(debtor.getPostalCode());
            destinationPo.setDebtorProvince(debtor.getProvince());
            destinationPo.setDebtorRegion(debtor.getRegion());
            destinationPo.setDebtorStreetName(debtor.getStreetName());
        }

        destinationPo.setPaymentPosition(destination);
        destinationPo.setDescription(source.getDescription());
        destinationPo.setOptionType(source.getInstallments().size() > 1 ? OptionType.OPZIONE_RATEALE : OptionType.OPZIONE_UNICA);
        destinationPo.setRetentionDate(source.getRetentionDate());

        mapAndUpdateInstallments(source, destinationPo, destination);
    }

    private void mapAndUpdateInstallments(
            PaymentOptionModelV3 source, PaymentOption destinationPo, PaymentPosition destination) {
        Map<String, Installment> managedInstallmentsByIuv =
                destinationPo.getInstallment().stream()
                        .collect(Collectors.toMap(Installment::getIuv, po -> po));

        List<InstallmentModel> sourceInstallments = source.getInstallments();
        List<Installment> installmentsToRemove = new ArrayList<>(destinationPo.getInstallment());

        if (sourceInstallments != null) {
            for (InstallmentModel sourceInstallment : sourceInstallments) {
                Installment managedInstallment = managedInstallmentsByIuv.get(sourceInstallment.getIuv());
                if (managedInstallment != null) {
                    // UPDATE: the installment
                    mapAndUpdateSingleInstallment(sourceInstallment, managedInstallment, destinationPo, destination);
                    installmentsToRemove.remove(managedInstallment);
                } else {
                    // CREATE: the installment
                    Installment inst = Installment.builder().build();
                    inst.setSendSync(false);
                    mapAndUpdateSingleInstallment(sourceInstallment, inst, destinationPo, destination);
                    destinationPo.getInstallment().add(inst);
                }
            }
        }

        // DELETE: remove the orphans installments
        destinationPo.getInstallment().removeAll(installmentsToRemove);
    }

    /**
     * @param source      the input model
     * @param destination the output entity
     */
    private void mapAndUpdateSingleInstallment(
            InstallmentModel source, Installment destinationInstallment, PaymentOption destinationPo, PaymentPosition destination) {
        destinationInstallment.setNav(source.getNav());
        destinationInstallment.setIuv(source.getIuv());
        destinationInstallment.setAmount(source.getAmount());
        destinationInstallment.setDescription(source.getDescription());
        destinationInstallment.setDueDate(source.getDueDate());
        destinationInstallment.setFee(source.getFee());
        destinationInstallment.setLastUpdatedDate(LocalDateTime.now());
        destinationInstallment.setPaymentOption(destinationPo);
        destinationInstallment.setPaymentPosition(destination);

        mapAndUpdateTransfers(source, destinationInstallment);
        // TODO METADATA mapAndUpdateOptionMetadata(source, destination);
    }

    private void mapAndUpdateTransfers(
            InstallmentModel sourceInstallment, Installment destinationInstallment) {
        Map<String, Transfer> managedTransfersById =
                destinationInstallment.getTransfer().stream()
                        .collect(Collectors.toMap(Transfer::getTransferId, t -> t));

        List<Transfer> transfersToRemove = new ArrayList<>(destinationInstallment.getTransfer());

        if (sourceInstallment.getTransfer() != null) {
            for (TransferModel sourceTx : sourceInstallment.getTransfer()) {
                Transfer managedTx = managedTransfersById.get(sourceTx.getIdTransfer());
                if (managedTx != null) {
                    // UPDATE
                    mapAndUpdateSingleTransfer(sourceTx, managedTx, destinationInstallment);
                    transfersToRemove.remove(managedTx);
                } else {
                    // CREATE
                    Transfer tr = Transfer.builder().build();
                    mapAndUpdateSingleTransfer(sourceTx, tr, destinationInstallment);
                    destinationInstallment.getTransfer().add(tr);
                }
            }
        }
        // DELETE
        destinationInstallment.getTransfer().removeAll(transfersToRemove);
    }

    private void mapAndUpdateSingleTransfer(TransferModel source, Transfer destinationTr, Installment destinationInst) {
        destinationTr.setAmount(source.getAmount());
        destinationTr.setCategory(source.getCategory());
        // TODO where has it gone  destinationTr.setCompanyName(source.getCompanyName());
        destinationTr.setIban(source.getIban());
        destinationTr.setTransferId(source.getIdTransfer());
        destinationTr.setLastUpdatedDate(LocalDateTime.now());
        destinationTr.setOrganizationFiscalCode(source.getOrganizationFiscalCode());
        destinationTr.setPostalIban(source.getPostalIban());
        destinationTr.setRemittanceInformation(source.getRemittanceInformation());
        Stamp stamp = source.getStamp();
        if (stamp != null) {
            destinationTr.setHashDocument(stamp.getHashDocument());
            destinationTr.setProvincialResidence(stamp.getProvincialResidence());
            destinationTr.setStampType(stamp.getStampType());
        }
        destinationTr.setInstallment(destinationInst);

        //TODO mapAndUpdateTransferMetadata(source, destinationTr);
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
