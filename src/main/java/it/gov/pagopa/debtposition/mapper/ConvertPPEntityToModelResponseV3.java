package it.gov.pagopa.debtposition.mapper;

import it.gov.pagopa.debtposition.entity.Installment;
import it.gov.pagopa.debtposition.entity.PaymentOption;
import it.gov.pagopa.debtposition.entity.PaymentPosition;
import it.gov.pagopa.debtposition.entity.Transfer;
import it.gov.pagopa.debtposition.model.pd.DebtorModel;
import it.gov.pagopa.debtposition.model.pd.Stamp;
import it.gov.pagopa.debtposition.model.pd.response.TransferMetadataModelResponse;
import it.gov.pagopa.debtposition.model.pd.response.TransferModelResponse;
import it.gov.pagopa.debtposition.model.v3.InstallmentMetadataModel;
import it.gov.pagopa.debtposition.model.v3.response.InstallmentModelResponse;
import it.gov.pagopa.debtposition.model.v3.response.PaymentOptionModelResponseV3;
import it.gov.pagopa.debtposition.model.v3.response.PaymentPositionModelResponseV3;
import it.gov.pagopa.debtposition.util.ObjectMapperUtils;
import org.modelmapper.Converter;
import org.modelmapper.spi.MappingContext;

import java.util.ArrayList;
import java.util.Objects;

public class ConvertPPEntityToModelResponseV3
        implements Converter<PaymentPosition, PaymentPositionModelResponseV3> {

    @Override
    public PaymentPositionModelResponseV3 convert(
            MappingContext<PaymentPosition, PaymentPositionModelResponseV3> context) {
        PaymentPosition source = context.getSource();
        PaymentPositionModelResponseV3 destination = new PaymentPositionModelResponseV3();

        destination.setIupd(source.getIupd());
        destination.setOrganizationFiscalCode(source.getOrganizationFiscalCode());
        destination.setCompanyName(source.getCompanyName());
        destination.setOfficeName(source.getOfficeName());
        destination.setInsertedDate(source.getInsertedDate());
        destination.setPublishDate(source.getPublishDate());
        destination.setPaymentDate(source.getPaymentDate());
        destination.setStatus(source.getStatus());

        if (source.getPaymentOption() != null) {
            destination.setPaymentOption(source.getPaymentOption().stream().filter(Objects::nonNull).map(this::convertPaymentOption).toList());
        } else {
            destination.setPaymentOption(new ArrayList<>());
        }

        return destination;
    }

    private PaymentOptionModelResponseV3 convertPaymentOption(PaymentOption po) {
        PaymentOptionModelResponseV3 pov3 = new PaymentOptionModelResponseV3();

        pov3.setValidityDate(po.getValidityDate());
        pov3.setRetentionDate(po.getRetentionDate());
        pov3.setInsertedDate(po.getInsertedDate());
        pov3.setSwitchToExpired(po.getSwitchToExpired());

        DebtorModel debtor = new DebtorModel();
        debtor.setType(po.getDebtorType());
        debtor.setFiscalCode(po.getDebtorFiscalCode());
        debtor.setFullName(po.getDebtorFullName());
        debtor.setStreetName(po.getDebtorStreetName());
        debtor.setCivicNumber(po.getDebtorCivicNumber());
        debtor.setPostalCode(po.getDebtorPostalCode());
        debtor.setCity(po.getDebtorCity());
        debtor.setProvince(po.getDebtorProvince());
        debtor.setRegion(po.getDebtorRegion());
        debtor.setCountry(po.getDebtorCountry());
        debtor.setEmail(po.getDebtorEmail());
        debtor.setPhone(po.getDebtorPhone());
        pov3.setDebtor(debtor);

        if (po.getInstallment() != null) {
            pov3.setInstallments(po.getInstallment().stream().filter(Objects::nonNull).map(this::convertInstallment).toList());
        } else {
            pov3.setInstallments(new ArrayList<>());
        }

        return pov3;
    }

    private InstallmentModelResponse convertInstallment(Installment installment) {
        InstallmentModelResponse installmentModel = new InstallmentModelResponse();

        installmentModel.setNav(installment.getNav());
        installmentModel.setIuv(installment.getIuv());
        installmentModel.setOrganizationFiscalCode(installment.getOrganizationFiscalCode());
        installmentModel.setAmount(installment.getAmount());
        installmentModel.setDescription(installment.getDescription());
        installmentModel.setDueDate(installment.getDueDate());
        installmentModel.setPaymentDate(installment.getPaymentDate());
        installmentModel.setReportingDate(installment.getReportingDate());
        installmentModel.setPaymentMethod(installment.getPaymentMethod());
        installmentModel.setPspCompany(installment.getPspCompany());
        installmentModel.setFee(installment.getFee());
        installmentModel.setNotificationFee(installment.getNotificationFee());
        installmentModel.setIdReceipt(installment.getReceiptId());
        installmentModel.setIdFlowReporting(installment.getFlowReportingId());
        installmentModel.setStatus(installment.getStatus());
        installmentModel.setLastUpdatedDate(installment.getLastUpdatedDate());
        if (installment.getTransfer() != null) {
            installmentModel.setTransfer(installment.getTransfer().stream().map(this::convertTransfer).toList());
        } else {
            installmentModel.setTransfer(new ArrayList<>());
        }

        installmentModel.setInstallmentMetadata(ObjectMapperUtils.mapAll(installment.getMetadata(), InstallmentMetadataModel.class));


        return installmentModel;
    }

    private TransferModelResponse convertTransfer(Transfer t) {
        TransferModelResponse destination = new TransferModelResponse();

        if (null == t) return destination;

        destination.setOrganizationFiscalCode(t.getOrganizationFiscalCode());
        destination.setCompanyName(t.getInstallment().getPaymentPosition().getCompanyName());
        destination.setIdTransfer(t.getTransferId());
        destination.setAmount(t.getAmount());
        destination.setRemittanceInformation(t.getRemittanceInformation());
        destination.setCategory(t.getCategory());
        destination.setIban(t.getIban());
        destination.setPostalIban(t.getPostalIban());

        // if one of Stamp attributes are different from null return Stamp values
        if (t.getHashDocument() != null
                || t.getStampType() != null
                || t.getProvincialResidence() != null) {
            destination.setStamp(
                    Stamp.builder()
                            .hashDocument(t.getHashDocument())
                            .provincialResidence(t.getProvincialResidence())
                            .stampType(t.getStampType())
                            .build());
        }
        destination.setInsertedDate(t.getInsertedDate());
        destination.setStatus(t.getStatus());
        destination.setLastUpdatedDate(t.getLastUpdatedDate());

        destination.setTransferMetadata(ObjectMapperUtils.mapAll(t.getMetadata(), TransferMetadataModelResponse.class));

        return destination;
    }
}
