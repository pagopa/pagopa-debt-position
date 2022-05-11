package it.gov.pagopa.debtposition.controller.pd.mapper;

import it.gov.pagopa.debtposition.entity.PaymentOption;
import it.gov.pagopa.debtposition.entity.PaymentPosition;
import it.gov.pagopa.debtposition.entity.Transfer;
import it.gov.pagopa.debtposition.model.pd.PaymentOptionModel;
import it.gov.pagopa.debtposition.model.pd.PaymentPositionModel;
import it.gov.pagopa.debtposition.model.pd.TransferModel;
import org.modelmapper.Converter;
import org.modelmapper.spi.MappingContext;

import javax.validation.constraints.NotNull;
import java.util.List;

public class ConvertPPModelToPPEntityForUpdate implements Converter<PaymentPositionModel, PaymentPosition> {

    @Override
    public PaymentPosition convert(MappingContext<@NotNull PaymentPositionModel, PaymentPosition> context) {
        PaymentPositionModel source = context.getSource();
        PaymentPosition destination = context.getDestination() != null ? context.getDestination() : new PaymentPosition();


        destination.setCity(source.getCity());
        destination.setCivicNumber(source.getCivicNumber());
        destination.setCompanyName(source.getCompanyName());
        destination.setCountry(source.getCountry());
        destination.setEmail(source.getEmail());
        destination.setFiscalCode(source.getFiscalCode());
        destination.setFullName(source.getFullName());
        destination.setIupd(source.getIupd());
        destination.setOfficeName(source.getOfficeName());
        destination.setPhone(source.getPhone());
        destination.setPostalCode(source.getPostalCode());
        destination.setProvince(source.getProvince());
        destination.setRegion(source.getRegion());
        destination.setStreetName(source.getStreetName());
        destination.setType(source.getType());
        destination.setValidityDate(source.getValidityDate());
        destination.setSwitchToExpired(null != source.getSwitchToExpired() && source.getSwitchToExpired());

        List<PaymentOptionModel> paymentOpts = source.getPaymentOption();
        if (null != paymentOpts && !paymentOpts.isEmpty()) {
            for (PaymentOptionModel pOModel : paymentOpts) {
                destination.addPaymentOption(this.convertPOModelToPOEntityForUpdate(pOModel));
            }
        }

        return destination;
    }

    private PaymentOption convertPOModelToPOEntityForUpdate(PaymentOptionModel pom) {

        PaymentOption po = new PaymentOption();
        po.setAmount(pom.getAmount());
        po.setDescription(pom.getDescription());
        po.setDueDate(pom.getDueDate());
        po.setFee(pom.getFee());
        po.setIsPartialPayment(pom.getIsPartialPayment());
        po.setIuv(pom.getIuv());
        po.setRetentionDate(pom.getRetentionDate());

        List<TransferModel> transfers = pom.getTransfer();
        if (null != transfers && !transfers.isEmpty()) {
            for (TransferModel tModel : transfers) {
                po.addTransfer(this.convertTransfModelToTransfEntityForUpdate(tModel));
            }
        }

        return po;

    }

    private Transfer convertTransfModelToTransfEntityForUpdate(TransferModel tm) {

        Transfer t = new Transfer();
        t.setAmount(tm.getAmount());
        t.setCategory(tm.getCategory());
        t.setIban(tm.getIban());
        t.setIdTransfer(tm.getIdTransfer());
        t.setPostalIban(tm.getPostalIban());
        t.setRemittanceInformation(tm.getRemittanceInformation());

        return t;
    }


}
