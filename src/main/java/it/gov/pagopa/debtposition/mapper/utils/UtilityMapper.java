package it.gov.pagopa.debtposition.mapper.utils;

import it.gov.pagopa.debtposition.entity.PaymentOption;
import it.gov.pagopa.debtposition.entity.Transfer;
import it.gov.pagopa.debtposition.model.enumeration.Type;
import it.gov.pagopa.debtposition.model.pd.DebtorModel;
import it.gov.pagopa.debtposition.model.pd.Stamp;
import it.gov.pagopa.debtposition.model.pd.response.TransferMetadataModelResponse;
import it.gov.pagopa.debtposition.model.pd.response.TransferModelResponse;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import org.springframework.util.CollectionUtils;

public class UtilityMapper {

  private UtilityMapper() {
    throw new IllegalStateException("Utility class");
  }

  public static final String UNDEFINED_DEBTOR = "NA";

  public static DebtorModel extractDebtor(PaymentOption po) {
    DebtorModel debtor = new DebtorModel();
    // Set default value for Type, FullName and FiscalCode (tax code) if they are null
    debtor.setType(Optional.ofNullable(po.getDebtorType()).orElse(Type.F));
    debtor.setFiscalCode(Optional.ofNullable(po.getDebtorFiscalCode()).orElse(UNDEFINED_DEBTOR));
    debtor.setFullName(Optional.ofNullable(po.getDebtorFullName()).orElse(UNDEFINED_DEBTOR));
    debtor.setStreetName(po.getDebtorStreetName());
    debtor.setCivicNumber(po.getDebtorCivicNumber());
    debtor.setPostalCode(po.getDebtorPostalCode());
    debtor.setCity(po.getDebtorCity());
    debtor.setProvince(po.getDebtorProvince());
    debtor.setRegion(po.getDebtorRegion());
    debtor.setCountry(po.getDebtorCountry());
    debtor.setEmail(po.getDebtorEmail());
    debtor.setPhone(po.getDebtorPhone());
    return debtor;
  }

  // Payment option metadata to Installment metadata converter
  // TODO METADATA
//  public static List<InstallmentMetadataModel> convert(List<PaymentOptionMetadata> poMetadata) {
//    List<InstallmentMetadataModel> installmentsMetadata = new ArrayList<>();
//    if (null != poMetadata && !CollectionUtils.isEmpty(poMetadata)) {
//      installmentsMetadata =
//          poMetadata.stream()
//              .map(
//                  m ->
//                      InstallmentMetadataModel.builder()
//                          .key(m.getKey())
//                          .value(m.getValue())
//                          .build())
//              .toList();
//    }
//    return installmentsMetadata;
//  }

  // Transfer metadata converters
  // TODO METADATA
//  public static List<TransferMetadataModel> convertTransferMetadata(
//      List<TransferMetadata> transferMetadata) {
//    if (CollectionUtils.isEmpty(transferMetadata)) {
//      return Collections.emptyList();
//    }
//    return transferMetadata.stream()
//        .map(m -> TransferMetadataModel.builder().key(m.getKey()).value(m.getValue()).build())
//        .collect(Collectors.toList());
//  }
//
//  public static List<TransferMetadataModelResponse> convertTransferMetadataResponse(
//      List<TransferMetadata> transferMetadata) {
//    List<TransferMetadataModelResponse> transferMetadataModelResponses = new ArrayList<>();
//    if (null != transferMetadata && !CollectionUtils.isEmpty(transferMetadata)) {
//      transferMetadataModelResponses =
//          transferMetadata.stream()
//              .map(
//                  m ->
//                      TransferMetadataModelResponse.builder()
//                          .key(m.getKey())
//                          .value(m.getValue())
//                          .build())
//              .toList();
//    }
//    return transferMetadataModelResponses;
//  }
}
