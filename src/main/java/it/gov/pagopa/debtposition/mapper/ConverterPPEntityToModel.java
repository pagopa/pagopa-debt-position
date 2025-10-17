package it.gov.pagopa.debtposition.mapper;

import it.gov.pagopa.debtposition.entity.Installment;
import it.gov.pagopa.debtposition.entity.PaymentOption;
import it.gov.pagopa.debtposition.entity.PaymentPosition;
import it.gov.pagopa.debtposition.entity.Transfer;
import it.gov.pagopa.debtposition.model.enumeration.OptionType;
import it.gov.pagopa.debtposition.model.pd.Stamp;
import it.gov.pagopa.debtposition.model.pd.TransferModel;
import it.gov.pagopa.debtposition.model.pd.PaymentOptionModel;
import it.gov.pagopa.debtposition.model.pd.PaymentPositionModel;
import it.gov.pagopa.debtposition.util.ObjectMapperUtils;
import org.modelmapper.Converter;
import org.modelmapper.spi.MappingContext;

import java.util.ArrayList;
import java.util.List;

public class ConverterPPEntityToModel
        implements Converter<PaymentPosition, PaymentPositionModel> {

    @Override
    public PaymentPositionModel convert(
            MappingContext<PaymentPosition, PaymentPositionModel> context) {
        PaymentPosition source = context.getSource();
        PaymentPositionModel destination = new PaymentPositionModel();

        destination.setIupd(source.getIupd());
        destination.setPayStandIn(source.getPayStandIn());
        destination.setCompanyName(source.getCompanyName());
        destination.setOfficeName(source.getOfficeName());
        destination.setValidityDate(source.getValidityDate());
        destination.setPaymentDate(source.getPaymentDate());
        destination.setStatus(ObjectMapperUtils.mapPpStatusV3ToPpStatus(source.getStatus()));

        if(source.getPaymentOption() != null){
            PaymentOption sourceFirstPO = source.getPaymentOption().get(0);
            destination.setType(sourceFirstPO.getDebtorType());
            destination.setFiscalCode(sourceFirstPO.getDebtorFiscalCode());
            destination.setFullName(sourceFirstPO.getDebtorFullName());
            destination.setStreetName(sourceFirstPO.getDebtorStreetName());
            destination.setCivicNumber(sourceFirstPO.getDebtorCivicNumber());
            destination.setPostalCode(sourceFirstPO.getDebtorPostalCode());
            destination.setCity(sourceFirstPO.getDebtorCity());
            destination.setProvince(sourceFirstPO.getDebtorProvince());
            destination.setRegion(sourceFirstPO.getDebtorRegion());
            destination.setCountry(sourceFirstPO.getDebtorCountry());
            destination.setEmail(sourceFirstPO.getDebtorEmail());
            destination.setPhone(sourceFirstPO.getDebtorPhone());
            destination.setSwitchToExpired(source.getPaymentOption().stream().anyMatch(po -> Boolean.TRUE.equals(po.getSwitchToExpired())));

            destination.setPaymentOption(convertPaymentOptions(source.getPaymentOption()));
        } else {
            destination.setPaymentOption(new ArrayList<>());
        }

        return destination;
    }

    private List<PaymentOptionModel> convertPaymentOptions(List<PaymentOption> sourcePoList) {
        List<PaymentOptionModel> destinationPoList = new ArrayList<>();

        for(PaymentOption sourcePO : sourcePoList){
            for(Installment sourceInst : sourcePO.getInstallment()){
                PaymentOptionModel destinationPO = new PaymentOptionModel();

                destinationPO.setNav(sourceInst.getNav());
                destinationPO.setIuv(sourceInst.getIuv());
                destinationPO.setAmount(sourceInst.getAmount());
                destinationPO.setDescription(sourcePO.getDescription());
                destinationPO.setIsPartialPayment(OptionType.OPZIONE_RATEALE.equals(sourcePO.getOptionType()));
                destinationPO.setDueDate(sourceInst.getDueDate());
                destinationPO.setRetentionDate(sourcePO.getRetentionDate());
                destinationPO.setFee(sourceInst.getFee());
                destinationPO.setNotificationFee(sourceInst.getNotificationFee());

                destinationPO.setTransfer(sourceInst.getTransfer().stream().map(this::convertTransfer).toList());

                destinationPoList.add(destinationPO);
            }
        }

        return destinationPoList;
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
