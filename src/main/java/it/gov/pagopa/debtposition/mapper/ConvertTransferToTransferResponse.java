package it.gov.pagopa.debtposition.mapper;

import it.gov.pagopa.debtposition.entity.Transfer;
import it.gov.pagopa.debtposition.model.pd.Stamp;
import it.gov.pagopa.debtposition.model.pd.response.TransferMetadataModelResponse;
import it.gov.pagopa.debtposition.model.pd.response.TransferModelResponse;
import it.gov.pagopa.debtposition.util.ObjectMapperUtils;

public class ConvertTransferToTransferResponse {

    public static TransferModelResponse convert(Transfer t) {
        TransferModelResponse destination = new TransferModelResponse();

        destination.setOrganizationFiscalCode(t.getOrganizationFiscalCode());
        destination.setIdTransfer(t.getIdTransfer());
        destination.setAmount(t.getAmount());
        destination.setRemittanceInformation(t.getRemittanceInformation());
        destination.setCategory(t.getCategory());
        destination.setIban(t.getIban());
        destination.setPostalIban(t.getPostalIban());
        destination.setInsertedDate(t.getInsertedDate());
        destination.setStatus(t.getStatus());
        destination.setLastUpdatedDate(t.getLastUpdatedDate());

        destination.setStamp(Stamp.builder()
                                     .hashDocument(t.getHashDocument())
                                     .provincialResidence(t.getProvincialResidence())
                                     .stampType(t.getStampType())
                                     .build());

        destination.setTransferMetadata(ObjectMapperUtils.mapAll(t.getTransferMetadata(), TransferMetadataModelResponse.class));

        return destination;
    }
}
