package it.gov.pagopa.debtposition.mapper;

import it.gov.pagopa.debtposition.entity.*;
import it.gov.pagopa.debtposition.mapper.utils.UtilityMapper;
import it.gov.pagopa.debtposition.model.enumeration.DebtPositionStatusV3;
import it.gov.pagopa.debtposition.model.enumeration.InstallmentStatus;
import it.gov.pagopa.debtposition.model.v3.response.InstallmentModelResponse;
import it.gov.pagopa.debtposition.model.v3.response.PaymentOptionModelResponseV3;
import it.gov.pagopa.debtposition.model.v3.response.PaymentPositionModelResponseV3;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;
import org.modelmapper.Converter;
import org.modelmapper.spi.MappingContext;

public class ConverterV3PPEntityToModelResponse
    implements Converter<PaymentPosition, PaymentPositionModelResponseV3> {

  @Override
  public PaymentPositionModelResponseV3 convert(
      MappingContext<PaymentPosition, PaymentPositionModelResponseV3> context) {
    PaymentPosition source = context.getSource();
    PaymentPositionModelResponseV3 destination = new PaymentPositionModelResponseV3();

    destination.setIupd(source.getIupd());
    destination.setOrganizationFiscalCode(source.getOrganizationFiscalCode());
    destination.setCompanyName(source.getCompanyName());
    destination.setOfficeName(source.getOfficeName());
    destination.setInsertedDate(source.getInsertedDate());
    destination.setPublishDate(source.getPublishDate());
    destination.setPaymentDate(source.getPaymentDate());
    destination.setLastUpdatedDate(source.getLastUpdatedDate());

    // check for status coherence and set
    String status = source.getStatus().name();
    String targetStatusV3 =
        switch (status) {
          case "EXPIRED", "INVALID" -> DebtPositionStatusV3.UNPAYABLE.toString();
          case "REPORTED" -> DebtPositionStatusV3.PAID.toString();
          default -> status;
        };
    destination.setStatus(DebtPositionStatusV3.valueOf(targetStatusV3));

    List<PaymentOption> paymentOptions = source.getPaymentOption();
    if (paymentOptions == null || paymentOptions.isEmpty()) {
      return destination;
    }

    // Partitioning the payment options into partial and unique POs
    Map<Boolean, List<PaymentOption>> partitionedPO =
        paymentOptions.stream()
            .collect(Collectors.partitioningBy(PaymentOption::getIsPartialPayment));

    // Extracting the partial and unique POs
    List<PaymentOption> partialPO = partitionedPO.get(true);
    List<PaymentOption> uniquePO = partitionedPO.get(false);
    List<PaymentOptionModelResponseV3> paymentOptionsToAdd = new ArrayList<>();
    
    if (partialPO != null && !partialPO.isEmpty()) {
    	// group partial payment-options by planId
    	Map<String, List<PaymentOption>> byPlan =
    			partialPO.stream().collect(Collectors.groupingBy(PaymentOption::getPaymentPlanId));

    	for (Map.Entry<String, List<PaymentOption>> entry : byPlan.entrySet()) {
    		List<PaymentOption> planInstallments = entry.getValue();
    		// if at least one installment of THIS plan has switchToExpired=true, the aggregated plan option is marked as switchToExpired.
    		boolean planAnyMarkedExpired = planInstallments.stream()
    				.anyMatch(i -> Boolean.TRUE.equals(i.getSwitchToExpired()));
    		PaymentOptionModelResponseV3 pov3 = this.convertPartialPO(source, planInstallments, planAnyMarkedExpired);
    		paymentOptionsToAdd.add(pov3);
    	}
    }
    
    if (null != uniquePO && !uniquePO.isEmpty()) {
    	List<PaymentOptionModelResponseV3> pov3List = uniquePO.stream()
    			.map(po -> convertUniquePO(source, po))
    			.toList();
    	paymentOptionsToAdd.addAll(pov3List);
    }
    
    // order by earliest dueDate among installments
	paymentOptionsToAdd
			.sort(Comparator.comparing(
					p -> p.getInstallments().stream().map(InstallmentModelResponse::getDueDate).filter(Objects::nonNull)
							.min(LocalDateTime::compareTo).orElse(null),
					Comparator.nullsLast(Comparator.naturalOrder())));

    destination.setPaymentOption(paymentOptionsToAdd);

    return destination;
  }

  // N partial PO -> 1 PaymentOption composed by N installment
  private PaymentOptionModelResponseV3 convertPartialPO(PaymentPosition pp, List<PaymentOption> partialPOs, boolean switchToExpired) {
    // Get only the first to fill common data for partial PO (retentionDate, insertedDate, debtor)
    PaymentOptionModelResponseV3 pov3 = convert(partialPOs.get(0));
    // todo re-enable when validityDate is read from payment option
//    // validityDate = min between the validity of the plan installments
//    LocalDateTime validityDate = partialPOs.stream()
//    	      .map(PaymentOption::getValidityDate)
//    	      .filter(Objects::nonNull)
//    	      .min(LocalDateTime::compareTo)
//    	      .orElse(null);
    pov3.setValidityDate(UtilityMapper.getValidityDate(pp, partialPOs));
    pov3.setSwitchToExpired(switchToExpired);
    // Set installments
    List<InstallmentModelResponse> installments =
    		partialPOs.stream()
    	    .sorted(Comparator.comparing(PaymentOption::getDueDate,
    	        Comparator.nullsLast(Comparator.naturalOrder())))
    	    .map(this::convertInstallment)
    	    .toList();
    pov3.setInstallments(installments);
    return pov3;
  }

  // 1 unique PO -> 1 PaymentOption composed by 1 installment
  private PaymentOptionModelResponseV3 convertUniquePO(PaymentPosition pp, PaymentOption po) {
    PaymentOptionModelResponseV3 pov3 = convert(po);
    pov3.setValidityDate(UtilityMapper.getValidityDate(pp, po));
    pov3.setSwitchToExpired(Boolean.TRUE.equals(po.getSwitchToExpired()));
    // set installment
    List<InstallmentModelResponse> installments = Collections.singletonList(convertInstallment(po));
    pov3.setInstallments(installments);
    return pov3;
  }

  private PaymentOptionModelResponseV3 convert(PaymentOption po) {
    PaymentOptionModelResponseV3 pov3 = new PaymentOptionModelResponseV3();

    pov3.setRetentionDate(po.getRetentionDate());
    pov3.setInsertedDate(po.getInsertedDate());
    pov3.setDebtor(UtilityMapper.extractDebtor(po));

    return pov3;
  }

  private InstallmentModelResponse convertInstallment(PaymentOption po) {
    InstallmentModelResponse inst = new InstallmentModelResponse();

    inst.setNav(po.getNav());
    inst.setIuv(po.getIuv());
    inst.setOrganizationFiscalCode(po.getOrganizationFiscalCode());
    inst.setAmount(po.getAmount());
    inst.setDescription(po.getDescription());
    inst.setDueDate(po.getDueDate());
    inst.setPaymentDate(po.getPaymentDate());
    inst.setReportingDate(po.getReportingDate());
    inst.setPaymentMethod(po.getPaymentMethod());
    inst.setPspCompany(po.getPspCompany());
    inst.setFee(po.getFee());
    inst.setNotificationFee(po.getNotificationFee());
    inst.setIdReceipt(po.getIdReceipt());
    inst.setIdFlowReporting(po.getIdFlowReporting());
    // substring to exclude prefix "PO_"
    String poStatus = po.getStatus().name();
    if (poStatus.startsWith("PO_")) {
      inst.setStatus(InstallmentStatus.valueOf(poStatus.substring(3)));
    } else {
      // this branch should never be executed because all payment option states are prefixed with
      // "PO_"
      inst.setStatus(InstallmentStatus.valueOf(poStatus));
    }
    inst.setLastUpdatedDate(po.getLastUpdatedDate());
    // set installment metadata
    inst.setInstallmentMetadata(UtilityMapper.convert(po.getPaymentOptionMetadata()));
    // set transfers
    List<Transfer> transfers = po.getTransfer();
    inst.setTransfer(UtilityMapper.convertTransfersToResponse(transfers));

    return inst;
  }
}
