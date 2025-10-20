package it.gov.pagopa.debtposition.mapper;

import it.gov.pagopa.debtposition.entity.Installment;
import it.gov.pagopa.debtposition.entity.PaymentOption;
import it.gov.pagopa.debtposition.entity.PaymentPosition;
import it.gov.pagopa.debtposition.entity.Transfer;
import it.gov.pagopa.debtposition.model.enumeration.OptionType;
import it.gov.pagopa.debtposition.model.pd.PaymentOptionModel;
import it.gov.pagopa.debtposition.model.pd.PaymentPositionModel;
import it.gov.pagopa.debtposition.model.pd.Stamp;
import it.gov.pagopa.debtposition.model.pd.TransferModel;
import jakarta.validation.constraints.NotNull;
import org.modelmapper.Converter;
import org.modelmapper.spi.MappingContext;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

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
        destination.setCompanyName(source.getCompanyName());
        destination.setOfficeName(source.getOfficeName());
        destination.setValidityDate(source.getValidityDate());

        List<PaymentOptionModel> sourcePOs = source.getPaymentOption();
        if (sourcePOs != null && !sourcePOs.isEmpty()) {
            mapAndUpdatePaymentOptions(source, destination);
        } else {
            destination.setPaymentOption(new ArrayList<>());
        }
    }

    private void mapAndUpdatePaymentOptions(
            PaymentPositionModel source, PaymentPosition destination) {
        List<String> sourceIUVs = new ArrayList<>(source.getPaymentOption().stream().map(PaymentOptionModel::getIuv).toList());

        // DELETE orphans installments & empty POs
        List<PaymentOption> paymentOptionsToDelete = new ArrayList<>();
        for (PaymentOption destinationPo : destination.getPaymentOption()) {
            List<Installment> installmentsToDelete = destinationPo.getInstallment().stream().filter(inst -> !sourceIUVs.contains(inst.getIuv())).toList();

            destinationPo.getInstallment().removeAll(installmentsToDelete);
            if (destinationPo.getInstallment().isEmpty()) {
                paymentOptionsToDelete.add(destinationPo);
            }
        }
        if(!paymentOptionsToDelete.isEmpty()){
            destination.getPaymentOption().removeAll(paymentOptionsToDelete);
        }

        // UPDATE existing POs
        for (PaymentOption destinationPo : destination.getPaymentOption()) {
            PaymentOptionModel sourcePOByIuv = source.getPaymentOption().stream().filter(
                    po -> destinationPo.getInstallment().stream().anyMatch(inst -> inst.getIuv().equals(po.getIuv()))).toList().get(0);;

            for (Installment installment : destinationPo.getInstallment()) {
                sourcePOByIuv = source.getPaymentOption().stream().filter(po -> po.getIuv().equals(installment.getIuv())).toList().get(0);

                mapAndUpdateInstallment(sourcePOByIuv, installment);
                sourceIUVs.remove(sourcePOByIuv.getIuv());
            }
            mapAndUpdateSinglePaymentOption(source, sourcePOByIuv, destinationPo);
        }

        // CREATE remaining POs
        List<PaymentOptionModel> remainingSourcePO = source.getPaymentOption().stream().filter(po -> sourceIUVs.contains(po.getIuv())).toList();
        // Partitioning the payment options into partial and unique POs
        Map<Boolean, List<PaymentOptionModel>> partitionedPO =
                remainingSourcePO.stream()
                        .collect(Collectors.partitioningBy(PaymentOptionModel::getIsPartialPayment));

        // Extracting the partial and unique POs
        List<PaymentOptionModel> partialPO = partitionedPO.get(true);
        List<PaymentOptionModel> uniquePO = partitionedPO.get(false);
        List<PaymentOption> paymentOptionsToAdd = new ArrayList<>();

        if (null != partialPO && !partialPO.isEmpty()) {
            paymentOptionsToAdd.add(this.createPartialPo(source, partialPO));
        }

        if (null != uniquePO && !uniquePO.isEmpty()) {
            List<PaymentOption> tempPoList =
                    uniquePO.stream().map(sourcePO -> this.createUniquePo(source, sourcePO)).toList();
            paymentOptionsToAdd.addAll(tempPoList);
        }

        destination.getPaymentOption().addAll(paymentOptionsToAdd);
    }

    private PaymentOption createUniquePo(
            PaymentPositionModel source, PaymentOptionModel sourcePO) {
        PaymentOption destinationPO = new PaymentOption();
        mapAndUpdateSinglePaymentOption(source, sourcePO, destinationPO);
        // set installment
        Installment destinationInstallment = new Installment();
        mapAndUpdateInstallment(sourcePO, destinationInstallment);

        List<Installment> installments = Collections.singletonList(destinationInstallment);
        destinationPO.setInstallment(installments);
        return destinationPO;
    }

    // N partial PO -> 1 PaymentOption composed by N installment
    private PaymentOption createPartialPo(
            PaymentPositionModel source, List<PaymentOptionModel> sourcePartialPOs) {
        // Get only the first to fill common data for partial PO (retentionDate, insertedDate, debtor)
        PaymentOption destinationPO = new PaymentOption();
        mapAndUpdateSinglePaymentOption(source, sourcePartialPOs.get(0), destinationPO);

        // Set installments
        List<Installment> installments =
                sourcePartialPOs.stream().map(sourcePO -> {
                    Installment destinationInstallment = new Installment();
                    mapAndUpdateInstallment(sourcePO, destinationInstallment);
                    return destinationInstallment;
                }).toList();
        destinationPO.setInstallment(installments);
        return destinationPO;
    }

    private void mapAndUpdateSinglePaymentOption(
            PaymentPositionModel source, PaymentOptionModel sourceOption, PaymentOption destinationPo) {
        destinationPo.setDescription(sourceOption.getDescription());
        destinationPo.setValidityDate(source.getValidityDate());
        destinationPo.setRetentionDate(sourceOption.getRetentionDate());
        destinationPo.setOptionType(Boolean.TRUE.equals(sourceOption.getIsPartialPayment()) ? OptionType.OPZIONE_RATEALE : OptionType.OPZIONE_UNICA);
        destinationPo.setSwitchToExpired(false); // TODO VERIFY
        destinationPo.setDebtorType(source.getType());
        destinationPo.setDebtorFiscalCode(source.getFiscalCode());
        destinationPo.setDebtorFullName(source.getFullName());
        destinationPo.setDebtorStreetName(source.getStreetName());
        destinationPo.setDebtorCivicNumber(source.getCivicNumber());
        destinationPo.setDebtorPostalCode(source.getPostalCode());
        destinationPo.setDebtorCity(source.getCity());
        destinationPo.setDebtorProvince(source.getProvince());
        destinationPo.setDebtorRegion(source.getRegion());
        destinationPo.setDebtorCountry(source.getCountry());
        destinationPo.setDebtorEmail(source.getEmail());
        destinationPo.setDebtorPhone(source.getPhone());
    }

    private void mapAndUpdateInstallment(PaymentOptionModel sourcePO, Installment destinationInstallment) {
        destinationInstallment.setNav(sourcePO.getNav());
        destinationInstallment.setIuv(sourcePO.getIuv());
        destinationInstallment.setAmount(sourcePO.getAmount());
        destinationInstallment.setDescription(sourcePO.getDescription());
        destinationInstallment.setDueDate(sourcePO.getDueDate());
        destinationInstallment.setFee(sourcePO.getFee());
        destinationInstallment.setLastUpdatedDate(LocalDateTime.now());

        List<String> sourceTransferIdList = new ArrayList<>(sourcePO.getTransfer().stream().map(TransferModel::getIdTransfer).toList());

        // DELETE orphans Transfers
        List<Transfer> transfersToDelete = destinationInstallment.getTransfer().stream().filter(tr -> !sourceTransferIdList.contains(tr.getTransferId())).toList();
        destinationInstallment.getTransfer().removeAll(transfersToDelete);
        sourceTransferIdList.removeAll(transfersToDelete.stream().map(Transfer::getTransferId).toList());

        // UPDATE existing Transfers
        for (Transfer destinationTransfer : destinationInstallment.getTransfer()) {
            TransferModel sourceTransfer = sourcePO.getTransfer().stream().filter(
                    tr -> tr.getIdTransfer().equals(destinationTransfer.getTransferId())).toList().get(0);

            mapAndUpdateSingleTransfer(sourceTransfer, destinationTransfer);

            sourceTransferIdList.remove(sourceTransfer.getIdTransfer());
        }
        // CREATE new Transfers
        List<TransferModel> remainingSourceTr = sourcePO.getTransfer().stream().filter(tr -> sourceTransferIdList.contains(tr.getIdTransfer())).toList();
        for (TransferModel sourceTransfer : remainingSourceTr) {
            Transfer destinationTransfer = new Transfer();
            mapAndUpdateSingleTransfer(sourceTransfer, destinationTransfer);

            destinationInstallment.getTransfer().add(destinationTransfer);
        }
    }

    private void mapAndUpdateSingleTransfer(TransferModel source, Transfer transferDestination) {
        transferDestination.setAmount(source.getAmount());
        transferDestination.setCategory(source.getCategory());
        // TODO where has it gone  destination.setCompanyName(source.getCompanyName());
        transferDestination.setIban(source.getIban());
        transferDestination.setTransferId(source.getIdTransfer());
        transferDestination.setLastUpdatedDate(LocalDateTime.now());
        transferDestination.setOrganizationFiscalCode(source.getOrganizationFiscalCode());
        transferDestination.setPostalIban(source.getPostalIban());
        transferDestination.setRemittanceInformation(source.getRemittanceInformation());
        Stamp stamp = source.getStamp();
        if (stamp != null) {
            transferDestination.setHashDocument(stamp.getHashDocument());
            transferDestination.setProvincialResidence(stamp.getProvincialResidence());
            transferDestination.setStampType(stamp.getStampType());
        }
        //TODO mapAndUpdateTransferMetadata(source, destination);
    }

//    private void mapAndUpdateOptionMetadata(PaymentOptionModel source, PaymentOption destination) {
//        Map<String, PaymentOptionMetadata> managedPaymentOptionMetadataByKey =
//                destination.getPaymentOptionMetadata().stream()
//                        .collect(Collectors.toMap(PaymentOptionMetadata::getKey, po -> po));
//
//        List<PaymentOptionMetadataModel> sourcePaymentOptionMetadata =
//                source.getPaymentOptionMetadata();
//        List<PaymentOptionMetadata> metadataToRemove =
//                new ArrayList<>(destination.getPaymentOptionMetadata());
//
//        if (sourcePaymentOptionMetadata != null) {
//            for (PaymentOptionMetadataModel sourceMetadata : sourcePaymentOptionMetadata) {
//                PaymentOptionMetadata managedMetadata =
//                        managedPaymentOptionMetadataByKey.get(sourceMetadata.getKey());
//
//                if (managedMetadata != null) {
//                    // UPDATE
//                    sourceMetadata.setValue(managedMetadata.getValue());
//                    metadataToRemove.remove(managedMetadata);
//                } else {
//                    // CREATE
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
//        // DELETE
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
//                    // UPDATE
//                    sourceMetadata.setValue(managedMetadata.getValue());
//                    metadataToRemove.remove(managedMetadata);
//                } else {
//                    // CREATE
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
//        // DELETE
//        destination.getTransferMetadata().removeAll(metadataToRemove);
//    }
}
