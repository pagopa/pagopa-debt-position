package it.gov.pagopa.debtposition.mapper;

import it.gov.pagopa.debtposition.entity.apd.PaymentOption;
import it.gov.pagopa.debtposition.entity.apd.PaymentPosition;
import it.gov.pagopa.debtposition.entity.apd.Transfer;
import it.gov.pagopa.debtposition.entity.odp.Installment;
import it.gov.pagopa.debtposition.entity.odp.PaymentOptionOdp;
import it.gov.pagopa.debtposition.entity.odp.PaymentPositionOdp;
import it.gov.pagopa.debtposition.entity.odp.TransferOdp;
import it.gov.pagopa.debtposition.model.enumeration.DebtPositionStatus;
import it.gov.pagopa.debtposition.model.enumeration.DebtPositionStatusV3;
import it.gov.pagopa.debtposition.model.enumeration.InstallmentStatus;
import it.gov.pagopa.debtposition.model.enumeration.OptionType;
import org.modelmapper.Converter;
import org.modelmapper.spi.MappingContext;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

public class ConverterPPEntityToPPOdpEntity
        implements Converter<PaymentPosition, PaymentPositionOdp> {

    @Override
    public PaymentPositionOdp convert(
            MappingContext<PaymentPosition, PaymentPositionOdp> context) {
        PaymentPosition source = context.getSource();
        PaymentPositionOdp destination = new PaymentPositionOdp();

        destination.setIupd(source.getIupd());
        destination.setOrganizationFiscalCode(source.getOrganizationFiscalCode());
        destination.setPull(source.getPull());
        destination.setPayStandIn(source.getPayStandIn());
        destination.setServiceType(source.getServiceType());
        destination.setCompanyName(source.getCompanyName());
        destination.setOfficeName(source.getOfficeName());
        destination.setInsertedDate(source.getInsertedDate());
        destination.setPublishDate(source.getPublishDate());
        destination.setMinDueDate(source.getMinDueDate());
        destination.setMaxDueDate(source.getMaxDueDate());
        destination.setLastUpdatedDate(source.getLastUpdatedDate());
        destination.setPaymentDate(source.getPaymentDate());
        destination.setVersion(source.getVersion());

        DebtPositionStatus sourceStatus = source.getStatus();
        destination.setStatus(sourceStatus.equals(DebtPositionStatus.EXPIRED) || sourceStatus.equals(DebtPositionStatus.INVALID) || sourceStatus.equals(DebtPositionStatus.REPORTED) ? DebtPositionStatusV3.UNPAYABLE : DebtPositionStatusV3.valueOf(source.getStatus().name()));
        destination.setValidityDate(source.getValidityDate());

        List<PaymentOption> paymentOptions = source.getPaymentOption();
        if (paymentOptions == null || paymentOptions.isEmpty()) {
            return destination;
        }

        // Partitioning the payment options into partial and unique POs
        Map<Boolean, List<PaymentOption>> partitionedPO =
                paymentOptions.stream()
                        .collect(Collectors.partitioningBy(PaymentOption::getIsPartialPayment));

        // Extracting the partial and unique POs
        List<PaymentOption> partialPO = partitionedPO.get(true);
        List<PaymentOption> uniquePO = partitionedPO.get(false);
        List<PaymentOptionOdp> paymentOptionsToAdd = new ArrayList<>();

        if (null != partialPO && !partialPO.isEmpty()) {
            paymentOptionsToAdd.add(this.convertPartialPO(partialPO, destination, source.getSwitchToExpired()));
        }

        if (null != uniquePO && !uniquePO.isEmpty()) {
            List<PaymentOptionOdp> pov3List =
                    uniquePO.stream().map(po -> convertUniquePO(po, destination, source.getSwitchToExpired())).toList();
            paymentOptionsToAdd.addAll(pov3List);
        }

        destination.setPaymentOptionOdp(paymentOptionsToAdd);

        return destination;
    }


    // 1 unique PO -> 1 PaymentOption composed by 1 installment
    private PaymentOptionOdp convertUniquePO(
            PaymentOption po, PaymentPositionOdp paymentPositionOdp, boolean switchToExpired) {
        PaymentOptionOdp paymentOptionOdp = convertPaymentOption(po, paymentPositionOdp, switchToExpired);
        // set installment
        List<Installment> installments = Collections.singletonList(convertInstallment(po, paymentPositionOdp, switchToExpired));
        paymentOptionOdp.setInstallments(installments);
        return paymentOptionOdp;
    }

    // N partial PO -> 1 PaymentOption composed by N installment
    private PaymentOptionOdp convertPartialPO(
            List<PaymentOption> partialPOs, PaymentPositionOdp paymentPositionOdp, boolean switchToExpired) {
        // Get only the first to fill common data for partial PO (retentionDate, insertedDate, debtor)
        PaymentOptionOdp paymentOptionOdp = convertPaymentOption(partialPOs.get(0), paymentPositionOdp, switchToExpired);
        // Set installments
        List<Installment> installments =
                partialPOs.stream().map(po -> convertInstallment(po, paymentPositionOdp, switchToExpired)).toList();
        paymentOptionOdp.setInstallments(installments);
        return paymentOptionOdp;
    }


    private PaymentOptionOdp convertPaymentOption(PaymentOption po, PaymentPositionOdp pp, Boolean switchToExpired) {
        return PaymentOptionOdp.builder()
                .organizationFiscalCode(po.getOrganizationFiscalCode())
                .description(po.getDescription())
                .validityDate(pp.getValidityDate())
                .retentionDate(po.getRetentionDate())
                .insertedDate(po.getInsertedDate())
                .paymentPositionOdp(pp)
                .optionType(po.getIsPartialPayment().equals(true) ? OptionType.OPZIONE_RATEALE : OptionType.OPZIONE_UNICA)
                .paymentPositionStatus(pp.getStatus())
                .switchToExpired(switchToExpired)
                .debtorType(po.getDebtorType())
                .debtorFiscalCode(po.getFiscalCode())
                .debtorFullName(po.getFullName())
                .debtorStreetName(po.getStreetName())
                .debtorCivicNumber(po.getCivicNumber())
                .debtorPostalCode(po.getPostalCode())
                .debtorCity(po.getCity())
                .debtorProvince(po.getProvince())
                .debtorRegion(po.getRegion())
                .debtorCountry(po.getCountry())
                .debtorEmail(po.getEmail())
                .debtorPhone(po.getPhone())
                .build();
    }

    private Installment convertInstallment(PaymentOption po, PaymentPositionOdp pp, Boolean switchToExpired) {
        String poStatus = po.getStatus().name();
        Installment installment = Installment.builder()
                .nav(po.getNav())
                .iuv(po.getIuv())
                .organizationFiscalCode(po.getOrganizationFiscalCode())
                .amount(po.getAmount())
                .description(po.getDescription())
                .dueDate(po.getDueDate())
                .paymentOptionOdp(convertPaymentOption(po, pp, switchToExpired))
                .paymentDate(po.getPaymentDate())
                .reportingDate(po.getReportingDate())
                .insertedDate(po.getInsertedDate())
                .paymentMethod(po.getPaymentMethod())
                .fee(po.getFee())
                .notificationFee(po.getNotificationFee())
                .pspCompany(po.getPspCompany())
                .pspCode(po.getPspCode())
                .pspTaxCode(po.getPspTaxCode())
                .receiptId(po.getIdReceipt())
                .flowReportingId(po.getIdFlowReporting())
                .status(poStatus.startsWith("PO_") ? InstallmentStatus.valueOf(poStatus.substring(3)) : InstallmentStatus.valueOf(poStatus))
                .lastUpdatedDate(po.getLastUpdatedDate())
                .lastUpdatedDateNotificationFee(po.getLastUpdatedDateNotificationFee())
                .sendSync(po.getSendSync())
                .paymentPositionOdp(pp)
                .build();
        installment.setTransferOdp(po.getTransfer().stream().map(el -> convertTransfer(el, installment)).toList());
        return installment;
    }

    private TransferOdp convertTransfer(Transfer transfer, Installment installment) {
        return TransferOdp.builder()
                .organizationFiscalCode(transfer.getOrganizationFiscalCode())
                .transferId(transfer.getIdTransfer())
                .iuv(transfer.getIuv())
                .amount(transfer.getAmount())
                .remittanceInformation(transfer.getRemittanceInformation())
                .category(transfer.getCategory())
                .iban(transfer.getIban())
                .postalIban(transfer.getPostalIban())
                .hashDocument(transfer.getHashDocument())
                .stampType(transfer.getStampType())
                .provincialResidence(transfer.getProvincialResidence())
                .installment(installment)
                .insertedDate(transfer.getInsertedDate())
                .status(transfer.getStatus())
                .lastUpdatedDate(transfer.getLastUpdatedDate())
                .build();
    }
}
