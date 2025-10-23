package it.gov.pagopa.debtposition.mapper;

import com.fasterxml.jackson.core.JsonProcessingException;
import it.gov.pagopa.debtposition.entity.Transfer;
import it.gov.pagopa.debtposition.exception.AppError;
import it.gov.pagopa.debtposition.exception.AppException;
import it.gov.pagopa.debtposition.model.pd.Stamp;
import it.gov.pagopa.debtposition.model.pd.response.TransferModelResponse;
import it.gov.pagopa.debtposition.util.ObjectMapperUtils;
import org.modelmapper.Converter;
import org.modelmapper.spi.MappingContext;

public class ConvertTransferEntityToTransferModeResponse implements Converter<Transfer, TransferModelResponse> {

    @Override
    public TransferModelResponse convert(MappingContext<Transfer, TransferModelResponse> context) {
        Transfer source = context.getSource();
        TransferModelResponse destination = new TransferModelResponse();

        destination.setOrganizationFiscalCode(source.getOrganizationFiscalCode());
        destination.setCompanyName(source.getInstallment().getPaymentPosition().getCompanyName());
        destination.setIdTransfer(source.getTransferId());
        destination.setAmount(source.getAmount());
        destination.setRemittanceInformation(source.getRemittanceInformation());
        destination.setCategory(source.getCategory());
        destination.setIban(source.getIban());
        destination.setPostalIban(source.getPostalIban());
        // if one of Stamp attributes are different from null return Stamp values
        if (source.getHashDocument() != null
                || source.getStampType() != null
                || source.getProvincialResidence() != null) {
            destination.setStamp(
                    Stamp.builder()
                            .hashDocument(source.getHashDocument())
                            .provincialResidence(source.getProvincialResidence())
                            .stampType(source.getStampType())
                            .build());
        }

        destination.setInsertedDate(source.getInsertedDate());
        destination.setStatus(source.getStatus());
        destination.setLastUpdatedDate(source.getLastUpdatedDate());
        try {
            destination.setTransferMetadata(ObjectMapperUtils.readValueList(source.getMetadata()));
        } catch (JsonProcessingException e) {
            throw new AppException(AppError.UNPROCESSABLE_ENTITY);
        }

        return destination;
    }
}