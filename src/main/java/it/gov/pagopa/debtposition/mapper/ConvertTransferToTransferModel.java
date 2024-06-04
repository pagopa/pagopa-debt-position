package it.gov.pagopa.debtposition.mapper;

import it.gov.pagopa.debtposition.entity.Transfer;
import it.gov.pagopa.debtposition.model.pd.Stamp;
import it.gov.pagopa.debtposition.model.pd.TransferMetadataModel;
import it.gov.pagopa.debtposition.model.pd.TransferModel;
import it.gov.pagopa.debtposition.util.ObjectMapperUtils;

public class ConvertTransferToTransferModel {

    public static TransferModel convert(Transfer t) {
        TransferModel destination = new TransferModel();

        destination.setOrganizationFiscalCode(t.getOrganizationFiscalCode());
        destination.setIdTransfer(t.getIdTransfer());
        destination.setAmount(t.getAmount());
        destination.setRemittanceInformation(t.getRemittanceInformation());
        destination.setCategory(t.getCategory());
        destination.setIban(t.getIban());
        destination.setPostalIban(t.getPostalIban());

        destination.setStamp(Stamp.builder()
                                     .hashDocument(t.getHashDocument())
                                     .provincialResidence(t.getProvincialResidence())
                                     .stampType(t.getStampType())
                                     .build());

        destination.setTransferMetadata(ObjectMapperUtils.mapAll(t.getTransferMetadata(), TransferMetadataModel.class));

        return destination;
    }
}
