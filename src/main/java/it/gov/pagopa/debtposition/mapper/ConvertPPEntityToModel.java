package it.gov.pagopa.debtposition.mapper;

import it.gov.pagopa.debtposition.entity.Installment;
import it.gov.pagopa.debtposition.entity.PaymentOption;
import it.gov.pagopa.debtposition.entity.PaymentPosition;
import it.gov.pagopa.debtposition.model.enumeration.OptionType;
import it.gov.pagopa.debtposition.model.pd.PaymentOptionMetadataModel;
import it.gov.pagopa.debtposition.model.pd.PaymentOptionModel;
import it.gov.pagopa.debtposition.model.pd.PaymentPositionModel;
import org.modelmapper.Converter;
import org.modelmapper.spi.MappingContext;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

public class ConvertPPEntityToModel
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
        destination.setStatus(source.getStatus());

        if (source.getPaymentOption() != null) {
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

        for (PaymentOption sourcePO : sourcePoList) {
            for (Installment sourceInst : sourcePO.getInstallment()) {
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

                destinationPO.setPaymentOptionMetadata(ConvertUtils.convertMetadataFromMap(sourceInst.getMetadata(), PaymentOptionMetadataModel.class));

                destinationPO.setTransfer(sourceInst.getTransfer().stream().filter(Objects::nonNull).map(ConvertUtils::convertTransferModel).toList());

                destinationPoList.add(destinationPO);
            }
        }

        return destinationPoList;
    }
}
