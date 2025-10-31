package it.gov.pagopa.debtposition.mapper;

import it.gov.pagopa.debtposition.entity.Transfer;
import it.gov.pagopa.debtposition.model.Metadata;
import it.gov.pagopa.debtposition.model.pd.Stamp;
import it.gov.pagopa.debtposition.model.pd.TransferModel;
import it.gov.pagopa.debtposition.model.pd.response.TransferModelResponse;

import java.util.*;
import java.util.stream.Collectors;

public class MapperUtils {
    MapperUtils() {
    }

    public static TransferModelResponse convertTransfer(Transfer sourceTransfer) {
        TransferModelResponse destination = new TransferModelResponse();

        destination.setOrganizationFiscalCode(sourceTransfer.getOrganizationFiscalCode());
        destination.setCompanyName(sourceTransfer.getCompanyName());
        destination.setIdTransfer(sourceTransfer.getTransferId());
        destination.setAmount(sourceTransfer.getAmount());
        destination.setRemittanceInformation(sourceTransfer.getRemittanceInformation());
        destination.setCategory(sourceTransfer.getCategory());
        destination.setIban(sourceTransfer.getIban());
        destination.setPostalIban(sourceTransfer.getPostalIban());
        // if one of Stamp attributes are different from null return Stamp values
        if (sourceTransfer.getHashDocument() != null
                || sourceTransfer.getStampType() != null
                || sourceTransfer.getProvincialResidence() != null) {
            destination.setStamp(
                    Stamp.builder()
                            .hashDocument(sourceTransfer.getHashDocument())
                            .provincialResidence(sourceTransfer.getProvincialResidence())
                            .stampType(sourceTransfer.getStampType())
                            .build());
        }

        destination.setInsertedDate(sourceTransfer.getInsertedDate());
        destination.setStatus(sourceTransfer.getStatus());
        destination.setLastUpdatedDate(sourceTransfer.getLastUpdatedDate());

        destination.setTransferMetadata(convertMetadataFromMap(sourceTransfer.getMetadata()));

        return destination;
    }

    public static TransferModel convertTransferModel(Transfer sourceTransfer) {
        TransferModel destination = new TransferModel();

        destination.setIdTransfer(sourceTransfer.getTransferId());
        destination.setAmount(sourceTransfer.getAmount());
        destination.setOrganizationFiscalCode(sourceTransfer.getOrganizationFiscalCode());
        destination.setRemittanceInformation(sourceTransfer.getRemittanceInformation());
        destination.setCategory(sourceTransfer.getCategory());
        destination.setIban(sourceTransfer.getIban());
        destination.setPostalIban(sourceTransfer.getPostalIban());
        // if one of Stamp attributes are different from null return Stamp values
        if (sourceTransfer.getHashDocument() != null
                || sourceTransfer.getStampType() != null
                || sourceTransfer.getProvincialResidence() != null) {
            destination.setStamp(
                    Stamp.builder()
                            .hashDocument(sourceTransfer.getHashDocument())
                            .provincialResidence(sourceTransfer.getProvincialResidence())
                            .stampType(sourceTransfer.getStampType())
                            .build());
        }

        destination.setCompanyName(sourceTransfer.getCompanyName());

        destination.setTransferMetadata(convertMetadataFromMap(sourceTransfer.getMetadata()));

        return destination;
    }

    public static Map<String, String> convertMetadataFromModel(List<Metadata> metadata) {

        return (metadata == null || metadata.isEmpty()) ?
                new LinkedHashMap<>() :
                metadata.stream()
                        .filter(Objects::nonNull)
                        .filter(m -> m.getKey() != null)
                        .collect(Collectors.toMap(
                                Metadata::getKey,
                                Metadata::getValue,
                                (oldValue, newValue) -> newValue,
                                LinkedHashMap::new
                        ));
    }

    public static List<Metadata> convertMetadataFromMap(Map<String, String> metadata) {
        return (metadata == null || metadata.isEmpty()) ?
                new ArrayList<>() :
                metadata.entrySet().stream()
                        .filter(Objects::nonNull)
                        .map(e -> new Metadata(e.getKey(), e.getValue()))
                        .toList();
    }
}
