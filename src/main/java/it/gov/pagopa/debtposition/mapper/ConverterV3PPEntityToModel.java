package it.gov.pagopa.debtposition.mapper;

import it.gov.pagopa.debtposition.entity.Installment;
import it.gov.pagopa.debtposition.entity.PaymentOption;
import it.gov.pagopa.debtposition.entity.PaymentPosition;
import it.gov.pagopa.debtposition.entity.Transfer;
import it.gov.pagopa.debtposition.model.pd.DebtorModel;
import it.gov.pagopa.debtposition.model.pd.Stamp;
import it.gov.pagopa.debtposition.model.pd.TransferModel;
import it.gov.pagopa.debtposition.model.v3.InstallmentModel;
import it.gov.pagopa.debtposition.model.v3.PaymentOptionModelV3;
import it.gov.pagopa.debtposition.model.v3.PaymentPositionModelV3;
import org.modelmapper.Converter;
import org.modelmapper.spi.MappingContext;

import java.util.ArrayList;

public class ConverterV3PPEntityToModel
        implements Converter<PaymentPosition, PaymentPositionModelV3> {

    @Override
    public PaymentPositionModelV3 convert(
            MappingContext<PaymentPosition, PaymentPositionModelV3> context) {
        PaymentPosition source = context.getSource();
        PaymentPositionModelV3 destination = new PaymentPositionModelV3();

        destination.setIupd(source.getIupd());
        destination.setPayStandIn(source.getPayStandIn());
        destination.setCompanyName(source.getCompanyName());
        destination.setOfficeName(source.getOfficeName());
        destination.setPaymentDate(source.getPaymentDate());
        destination.setStatus(source.getStatus());

        if(source.getPaymentOption() != null){
            destination.setPaymentOption(source.getPaymentOption().stream().map(this::convertPaymentOption).toList());
        } else {
            destination.setPaymentOption(new ArrayList<>());
        }

        return destination;
    }

    private PaymentOptionModelV3 convertPaymentOption(PaymentOption po) {
        PaymentOptionModelV3 pov3 = new PaymentOptionModelV3();

        if(po == null) return pov3; // TODO VERIFY

        pov3.setDescription(po.getDescription());
        pov3.setValidityDate(po.getValidityDate());
        pov3.setRetentionDate(po.getRetentionDate());
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

        if(po.getInstallment() != null){
            pov3.setInstallments(po.getInstallment().stream().map(this::convertInstallment).toList());
        } else {
            pov3.setInstallments(new ArrayList<>());
        }

        return pov3;
    }

    private InstallmentModel convertInstallment(Installment installment) {
        InstallmentModel installmentModel = new InstallmentModel();

        if (null == installment) return installmentModel; // TODO VERIFY

        installmentModel.setNav(installment.getNav());
        installmentModel.setIuv(installment.getIuv());
        installmentModel.setAmount(installment.getAmount());
        installmentModel.setDescription(installment.getDescription());
        installmentModel.setDueDate(installment.getDueDate());
        installmentModel.setFee(installment.getFee());
        installmentModel.setNotificationFee(installment.getNotificationFee());
        installmentModel.setStatus(installment.getStatus());
        //TODO metadata
        if(installment.getTransfer() != null){
            installmentModel.setTransfer(installment.getTransfer().stream().map(this::convertTransfer).toList());
        } else {
            installmentModel.setTransfer(new ArrayList<>());
        }

        return installmentModel;
    }

    private TransferModel convertTransfer(Transfer t) {
        TransferModel destination = new TransferModel();

        if (null == t) return destination;

        destination.setIdTransfer(t.getTransferId());
        destination.setAmount(t.getAmount());
        destination.setOrganizationFiscalCode(t.getOrganizationFiscalCode());
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

        destination.setCompanyName(t.getInstallment().getPaymentPosition().getCompanyName());

        // TODO
//    destination.setTransferMetadata(
//        ObjectMapperUtils.mapAll(t.getTransferMetadata(), TransferMetadataModel.class));
//
//    List<TransferMetadataModel> transferMetadataModelResponses =
//        convertTransferMetadata(t.getTransferMetadata());
//    destination.setTransferMetadata(transferMetadataModelResponses);

        return destination;
    }
}
