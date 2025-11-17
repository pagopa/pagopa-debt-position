package it.gov.pagopa.debtposition.mapper.utils;

import it.gov.pagopa.debtposition.entity.*;
import it.gov.pagopa.debtposition.exception.AppError;
import it.gov.pagopa.debtposition.exception.AppException;
import it.gov.pagopa.debtposition.model.enumeration.Type;
import it.gov.pagopa.debtposition.model.pd.DebtorModel;
import it.gov.pagopa.debtposition.model.pd.Stamp;
import it.gov.pagopa.debtposition.model.pd.TransferMetadataModel;
import it.gov.pagopa.debtposition.model.pd.TransferModel;
import it.gov.pagopa.debtposition.model.pd.response.TransferMetadataModelResponse;
import it.gov.pagopa.debtposition.model.pd.response.TransferModelResponse;
import it.gov.pagopa.debtposition.model.v3.InstallmentMetadataModel;
import it.gov.pagopa.debtposition.util.ObjectMapperUtils;

import java.time.LocalDateTime;
import java.util.*;
import java.util.stream.Collectors;

import org.springframework.util.CollectionUtils;

public class UtilityMapper {

  public static final String UNDEFINED_DEBTOR = "NA";

  public static DebtorModel extractDebtor(PaymentOption po) {
    DebtorModel debtor = new DebtorModel();
    // Set default value for Type, FullName and FiscalCode (tax code) if they are null
    debtor.setType(Optional.ofNullable(po.getDebtorType()).orElse(Type.F));
    debtor.setFiscalCode(Optional.ofNullable(po.getFiscalCode()).orElse(UNDEFINED_DEBTOR));
    debtor.setFullName(Optional.ofNullable(po.getFullName()).orElse(UNDEFINED_DEBTOR));
    debtor.setStreetName(po.getStreetName());
    debtor.setCivicNumber(po.getCivicNumber());
    debtor.setPostalCode(po.getPostalCode());
    debtor.setCity(po.getCity());
    debtor.setProvince(po.getProvince());
    debtor.setRegion(po.getRegion());
    debtor.setCountry(po.getCountry());
    debtor.setEmail(po.getEmail());
    debtor.setPhone(po.getPhone());
    return debtor;
  }

  // Transfer entity, model, response converters

  public static List<TransferModel> convertTransfers(List<Transfer> transfers) {
    List<TransferModel> transfersModel = new ArrayList<>();
    if (null != transfers && !CollectionUtils.isEmpty(transfers)) {
      transfersModel = transfers.stream().map(UtilityMapper::convert).toList();
    }
    return transfersModel;
  }

  public static List<TransferModelResponse> convertTransfersToResponse(List<Transfer> transfers) {
    List<TransferModelResponse> transfersModel = new ArrayList<>();
    if (null != transfers && !CollectionUtils.isEmpty(transfers)) {
      transfersModel = transfers.stream().map(UtilityMapper::convertToResponse).toList();
    }
    return transfersModel;
  }

  public static List<Transfer> convertTransfersModel(List<TransferModel> transfersModel) {
    if (CollectionUtils.isEmpty(transfersModel)) {
      return Collections.emptyList();
    }
    return transfersModel.stream().map(UtilityMapper::convert).toList();
  }

  // Payment option metadata to Installment metadata converter

  public static List<InstallmentMetadataModel> convert(List<PaymentOptionMetadata> poMetadata) {
    List<InstallmentMetadataModel> installmentsMetadata = new ArrayList<>();
    if (null != poMetadata && !CollectionUtils.isEmpty(poMetadata)) {
      installmentsMetadata =
          poMetadata.stream()
              .map(
                  m ->
                      InstallmentMetadataModel.builder()
                          .key(m.getKey())
                          .value(m.getValue())
                          .build())
              .toList();
    }
    return installmentsMetadata;
  }

  // Transfer metadata converters

  public static List<TransferMetadataModel> convertTransferMetadata(
      List<TransferMetadata> transferMetadata) {
    if (CollectionUtils.isEmpty(transferMetadata)) {
      return Collections.emptyList();
    }
    return transferMetadata.stream()
        .map(m -> TransferMetadataModel.builder().key(m.getKey()).value(m.getValue()).build())
        .collect(Collectors.toList());
  }

  public static List<TransferMetadataModelResponse> convertTransferMetadataResponse(
      List<TransferMetadata> transferMetadata) {
    List<TransferMetadataModelResponse> transferMetadataModelResponses = new ArrayList<>();
    if (null != transferMetadata && !CollectionUtils.isEmpty(transferMetadata)) {
      transferMetadataModelResponses =
          transferMetadata.stream()
              .map(
                  m ->
                      TransferMetadataModelResponse.builder()
                          .key(m.getKey())
                          .value(m.getValue())
                          .build())
              .toList();
    }
    return transferMetadataModelResponses;
  }

  // ##################################
  // #######  Private methods #########
  // ##################################
  private static TransferModel convert(Transfer t) {
    TransferModel destination = new TransferModel();

    if (null == t) return destination;

    destination.setOrganizationFiscalCode(t.getOrganizationFiscalCode());
    destination.setCompanyName(t.getCompanyName());
    destination.setIdTransfer(t.getIdTransfer());
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

    destination.setTransferMetadata(
        ObjectMapperUtils.mapAll(t.getTransferMetadata(), TransferMetadataModel.class));

    List<TransferMetadataModel> transferMetadataModelResponses =
        convertTransferMetadata(t.getTransferMetadata());
    destination.setTransferMetadata(transferMetadataModelResponses);

    return destination;
  }

  private static Transfer convert(TransferModel tm) {

    Transfer t = new Transfer();
    t.setAmount(tm.getAmount());
    t.setOrganizationFiscalCode(tm.getOrganizationFiscalCode());
    t.setCompanyName(tm.getCompanyName());
    t.setCategory(tm.getCategory());
    t.setIban(tm.getIban());
    t.setIdTransfer(tm.getIdTransfer());
    t.setPostalIban(tm.getPostalIban());
    if (tm.getStamp() != null) {
      t.setHashDocument(tm.getStamp().getHashDocument());
      t.setStampType(tm.getStamp().getStampType());
      t.setProvincialResidence(tm.getStamp().getProvincialResidence());
    }
    t.setRemittanceInformation(tm.getRemittanceInformation());

    List<TransferMetadataModel> metadata = tm.getTransferMetadata();
    if (!CollectionUtils.isEmpty(metadata)) {
      for (TransferMetadataModel m : metadata) {
        t.addTransferMetadata(
            TransferMetadata.builder().key(m.getKey()).value(m.getValue()).build());
      }
    }

    return t;
  }

  private static TransferModelResponse convertToResponse(Transfer t) {
    TransferModelResponse destination = new TransferModelResponse();

    destination.setOrganizationFiscalCode(t.getOrganizationFiscalCode());
    destination.setCompanyName(t.getCompanyName());
    destination.setIdTransfer(t.getIdTransfer());
    destination.setAmount(t.getAmount());
    destination.setRemittanceInformation(t.getRemittanceInformation());
    destination.setCategory(t.getCategory());
    destination.setIban(t.getIban());
    destination.setPostalIban(t.getPostalIban());
    destination.setInsertedDate(t.getInsertedDate());
    destination.setStatus(t.getStatus());
    destination.setLastUpdatedDate(t.getLastUpdatedDate());

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

    List<TransferMetadataModelResponse> transferMetadataModelResponses =
        convertTransferMetadataResponse(t.getTransferMetadata());
    destination.setTransferMetadata(transferMetadataModelResponses);

    return destination;
  }

  public static LocalDateTime getValidityDate(PaymentOption po) {
	  return po.getValidityDate();
  }

  public static boolean getSwitchToExpired(PaymentOption po) {
	  return po.getSwitchToExpired();
  }

  public static LocalDateTime getValidityDate(List<PaymentOption> po) {
	  // validityDate = min between the validity of the plan installments
	  return po.stream()
			  .map(PaymentOption::getValidityDate)
			  .filter(Objects::nonNull)
			  .min(LocalDateTime::compareTo)
			  .orElse(null);
  }

  public static boolean getSwitchToExpired(PaymentPosition pp) {
	  // the plan is marked expired if at least one installment is flagged expired (switchToExpired == TRUE)
	  return pp.getPaymentOption() != null && !pp.getPaymentOption().isEmpty() && pp.getPaymentOption()
			  .stream().anyMatch(po -> Boolean.TRUE.equals(po.getSwitchToExpired()));
  }

  /**
   * Groups a list of PaymentOption by 'paymentPlanId'.
   *
   * @param partialPO A list of partial PaymentOption.
   * @return A Map where keys are grouping criteria (String) and values are lists of PaymentOptions.
   * */
  public static Map<String, List<PaymentOption>> groupByPlanId(List<PaymentOption> partialPO) {
	  if (partialPO == null || partialPO.isEmpty()) {
		  return Collections.emptyMap();
	  }

	  // Validate all partial installments have a non-blank paymentPlanId; if missing, raises an exception
	  validateAllHavePlanId(partialPO);

	  // default grouping by payment_plan_id
	  return partialPO.stream().collect(Collectors.groupingBy(PaymentOption::getPaymentPlanId));
  }

  private static void validateAllHavePlanId(List<PaymentOption> partialPO) {
      for (PaymentOption po : partialPO) {
          String pid = po.getPaymentPlanId();
          if (pid == null || pid.isBlank()) {
              throw new AppException(
                      AppError.PAYMENT_PLAN_ID_MISSING,
                      String.valueOf(po.getIuv()),
                      String.valueOf(po.getOrganizationFiscalCode())
              );
          }
      }
  }
}
