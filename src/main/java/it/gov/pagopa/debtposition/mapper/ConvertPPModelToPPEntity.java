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
import java.util.*;
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
            mapAndUpdatePaymentOptions(source, sourcePOs, destination);
        } else {
            destination.setPaymentOption(null);
        }
    }

    private void mapAndUpdatePaymentOptions(PaymentPositionModel source, List<PaymentOptionModel> sourcePOs, PaymentPosition destination) {
        // Partitioning the payment options into partial and unique POs
        Map<Boolean, List<PaymentOptionModel>> partitionedPO =
                sourcePOs.stream()
                        .collect(Collectors.partitioningBy(PaymentOptionModel::getIsPartialPayment));

        // Extracting the partial and unique POs
        List<PaymentOptionModel> partialPO = partitionedPO.get(true);
        List<PaymentOptionModel> uniquePO = partitionedPO.get(false);
        List<PaymentOption> paymentOptionsToAdd = new ArrayList<>();

        if (null != partialPO && !partialPO.isEmpty()) {
            paymentOptionsToAdd.add(this.mapAndUpdatePartialPO(source, partialPO, destination));
        }

        if (null != uniquePO && !uniquePO.isEmpty()) {
            List<PaymentOption> tempPoList =
                    uniquePO.stream().map(sourcePO -> mapAndUpdateUniquePO(source, sourcePO, destination)).toList();
            paymentOptionsToAdd.addAll(tempPoList);
        }

        destination.setPaymentOption(paymentOptionsToAdd);
    }

    // 1 unique PO -> 1 PaymentOption composed by 1 installment
    private PaymentOption mapAndUpdateUniquePO(
            PaymentPositionModel source, PaymentOptionModel sourcePO, PaymentPosition destination) {
        PaymentOption destinationPO = mapAndUpdateSinglePaymentOption(source, sourcePO, destination);
        // set installment
        List<Installment> installments = Collections.singletonList(convertInstallment(sourcePO, destinationPO));
        destinationPO.setInstallment(installments);
        return destinationPO;
    }

    // N partial PO -> 1 PaymentOption composed by N installment
    private PaymentOption mapAndUpdatePartialPO(
            PaymentPositionModel source, List<PaymentOptionModel> sourcePartialPOs, PaymentPosition destination) {
        // Get only the first to fill common data for partial PO (retentionDate, insertedDate, debtor)
        PaymentOption destinationPO = mapAndUpdateSinglePaymentOption(source, sourcePartialPOs.get(0), destination);

        // Set installments
        List<Installment> installments =
                sourcePartialPOs.stream().map(sourcePO -> convertInstallment(sourcePO, destinationPO)).toList();
        destinationPO.setInstallment(installments);
        return destinationPO;
    }

    private PaymentOption mapAndUpdateSinglePaymentOption(
            PaymentPositionModel source, PaymentOptionModel sourceOption, PaymentPosition destination) {
        PaymentOption destinationPo = destination.getPaymentOption().stream().filter(po -> po.getInstallment().stream().anyMatch(inst -> inst.getIuv() == sourceOption.getIuv())).findFirst().orElse(new PaymentOption());

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

        return destinationPo;
    }

    private Installment convertInstallment(PaymentOptionModel sourcePO, PaymentOption destinationPo) {
        Installment destinationInstallment = destinationPo.getInstallment().stream().filter(inst -> Objects.equals(inst.getIuv(), sourcePO.getIuv())).findFirst().orElse(new Installment());
        destinationInstallment.setNav(sourcePO.getNav());
        destinationInstallment.setIuv(sourcePO.getIuv());
        destinationInstallment.setAmount(sourcePO.getAmount());
        destinationInstallment.setDescription(sourcePO.getDescription());
        destinationInstallment.setDueDate(sourcePO.getDueDate());
        destinationInstallment.setFee(sourcePO.getFee());
        destinationInstallment.setLastUpdatedDate(LocalDateTime.now());

        destinationInstallment.setTransfer(sourcePO.getTransfer().stream().map(sourceTr -> mapSingleTransfer(sourceTr, destinationInstallment)).toList());
        return destinationInstallment;
    }

    private Transfer mapSingleTransfer(TransferModel source, Installment destinationInstallment) {
        Transfer transferDestination = destinationInstallment.getTransfer().stream().filter(tr -> Objects.equals(tr.getTransferId(), source.getIdTransfer())).findFirst().orElse(new Transfer());
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

        return transferDestination;

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
