package it.gov.pagopa.debtposition.mapper;

import it.gov.pagopa.debtposition.entity.*;
import it.gov.pagopa.debtposition.model.pd.*;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;
import jakarta.validation.constraints.NotNull;
import org.modelmapper.Converter;
import org.modelmapper.spi.MappingContext;

public class ConvertPPModelToPPEntity implements Converter<PaymentPositionModel, PaymentPosition> {

  @Override
  public PaymentPosition convert(
      MappingContext<@NotNull PaymentPositionModel, PaymentPosition> context) {
    PaymentPositionModel source = context.getSource();
    PaymentPosition destination =
        context.getDestination() != null ? context.getDestination() : new PaymentPosition();

    mapAndUpdatePaymentPosition(source, destination);

    return destination;
  }

  public void mapAndUpdatePaymentPosition(
      PaymentPositionModel source, PaymentPosition destination) {

    destination.setIupd(source.getIupd());
    destination.setPayStandIn(source.isPayStandIn());
    destination.setType(source.getType());
    destination.setFiscalCode(source.getFiscalCode());
    destination.setFullName(source.getFullName());
    destination.setStreetName(source.getStreetName());
    destination.setCivicNumber(source.getCivicNumber());
    destination.setPostalCode(source.getPostalCode());
    destination.setCity(source.getCity());
    destination.setProvince(source.getProvince());
    destination.setRegion(source.getRegion());
    destination.setCountry(source.getCountry());
    destination.setEmail(source.getEmail());
    destination.setPhone(source.getPhone());
    destination.setCompanyName(source.getCompanyName());
    destination.setOfficeName(source.getOfficeName());
    // todo validityDate mapping remove after v1.1.0 promotion
    destination.setValidityDate(source.getValidityDate());
    // todo switchToExpired mapping remove after v1.1.0 promotion
    destination.setSwitchToExpired(source.getSwitchToExpired());
    
    mapAndUpdatePaymentOptions(source, destination);
  }
 
  private void mapAndUpdatePaymentOptions(
		  PaymentPositionModel source, PaymentPosition destination) {

	  Map<String, PaymentOption> managedOptionsByIuv =
			  destination.getPaymentOption().stream()
			  .collect(Collectors.toMap(PaymentOption::getIuv, po -> po));

	  List<PaymentOptionModel> sourceOptions = source.getPaymentOption();
	  List<PaymentOption> optionsToRemove = new ArrayList<>(destination.getPaymentOption());

	  // V1: all installment of the same debt position plan share one UUID
	  String sharedPlanId = null;
	  boolean hasAnyPlan =
			  sourceOptions != null && sourceOptions.stream().anyMatch(o -> Boolean.TRUE.equals(o.getIsPartialPayment()));
	  if (hasAnyPlan) {
		  sharedPlanId = findExistingSharedPlanUuid(managedOptionsByIuv.values())
				  .orElseGet(() -> java.util.UUID.randomUUID().toString());
	  }

	  if (sourceOptions != null) {
		  for (PaymentOptionModel sourceOpt : sourceOptions) {
			  PaymentOption managedOpt = managedOptionsByIuv.get(sourceOpt.getIuv());

			  if (managedOpt != null) {
				  // UPDATE
				  mapAndUpdateSinglePaymentOption(source, sourceOpt, managedOpt, sharedPlanId);
				  optionsToRemove.remove(managedOpt);
			  } else {
				  // CREATE
				  PaymentOption po = PaymentOption.builder().build();
				  po.setSendSync(false);
				  mapAndUpdateSinglePaymentOption(source, sourceOpt, po, sharedPlanId);
				  destination.getPaymentOption().add(po);
			  }
		  }
	  }

	  // DELETE orphans
	  destination.getPaymentOption().removeAll(optionsToRemove);
  }


  private void mapAndUpdateSinglePaymentOption(
		    PaymentPositionModel paymentPosition,
		    PaymentOptionModel source,
		    PaymentOption destination,
		    String sharedPlanId) {

    destination.setAmount(source.getAmount());
    destination.setCity(paymentPosition.getCity());
    destination.setCivicNumber(paymentPosition.getCivicNumber());
    destination.setCountry(paymentPosition.getCountry());
    destination.setDebtorType(paymentPosition.getType());
    destination.setDescription(source.getDescription());
    destination.setDueDate(source.getDueDate());
    destination.setEmail(paymentPosition.getEmail());
    destination.setFiscalCode(paymentPosition.getFiscalCode());
    destination.setFullName(paymentPosition.getFullName());
    destination.setIsPartialPayment(source.getIsPartialPayment());
    destination.setIuv(source.getIuv());
    destination.setLastUpdatedDate(LocalDateTime.now());
    destination.setNav(source.getNav());
    destination.setPhone(paymentPosition.getPhone());
    destination.setPostalCode(paymentPosition.getPostalCode());
    destination.setProvince(paymentPosition.getProvince());
    destination.setRegion(paymentPosition.getRegion());
    destination.setValidityDate(paymentPosition.getValidityDate());
    destination.setRetentionDate(source.getRetentionDate());
    destination.setStreetName(paymentPosition.getStreetName());
    
    // Propagate the parent flag on the installment that will persist on DB
    destination.setSwitchToExpired(Boolean.TRUE.equals(paymentPosition.getSwitchToExpired()));
    
    // Assign payment_plan_id (V1: shared among all isPartialPayment=true)
    if (Boolean.TRUE.equals(source.getIsPartialPayment())) {
    	if (!java.util.Objects.equals(destination.getPaymentPlanId(), sharedPlanId)) {
    		destination.setPaymentPlanId(sharedPlanId);
    	}
    } else {
    	if (destination.getPaymentPlanId() != null) {
    		destination.setPaymentPlanId(null);
    	}
    }

    mapAndUpdateTransfers(source, destination);
    mapAndUpdateOptionMetadata(source, destination);
  }

  private void mapAndUpdateTransfers(PaymentOptionModel source, PaymentOption destination) {
    Map<String, Transfer> managedTransfersById =
        destination.getTransfer().stream()
            .collect(Collectors.toMap(Transfer::getIdTransfer, t -> t));

    List<TransferModel> sourceTransfers = source.getTransfer();
    List<Transfer> transfersToRemove = new ArrayList<>(destination.getTransfer());

    if (sourceTransfers != null) {
      for (TransferModel sourceTx : sourceTransfers) {
        Transfer managedTx = managedTransfersById.get(sourceTx.getIdTransfer());
        if (managedTx != null) {
          // UPDATE
          mapAndUpdateSingleTransfer(sourceTx, managedTx);
          transfersToRemove.remove(managedTx);
        } else {
          // CREATE
          Transfer tr = Transfer.builder().build();
          mapAndUpdateSingleTransfer(sourceTx, tr);
          destination.getTransfer().add(tr);
        }
      }
    }
    // DELETE
    destination.getTransfer().removeAll(transfersToRemove);
  }

  private void mapAndUpdateSingleTransfer(TransferModel source, Transfer destination) {
    destination.setAmount(source.getAmount());
    destination.setCategory(source.getCategory());
    destination.setCompanyName(source.getCompanyName());
    destination.setIban(source.getIban());
    destination.setIdTransfer(source.getIdTransfer());
    destination.setLastUpdatedDate(LocalDateTime.now());
    destination.setOrganizationFiscalCode(source.getOrganizationFiscalCode());
    destination.setPostalIban(source.getPostalIban());
    destination.setRemittanceInformation(source.getRemittanceInformation());
    Stamp stamp = source.getStamp();
    if (stamp != null) {
      destination.setHashDocument(stamp.getHashDocument());
      destination.setProvincialResidence(stamp.getProvincialResidence());
      destination.setStampType(stamp.getStampType());
    }

    mapAndUpdateTransferMetadata(source, destination);
  }

  private void mapAndUpdateOptionMetadata(PaymentOptionModel source, PaymentOption destination) {
    Map<String, PaymentOptionMetadata> managedPaymentOptionMetadataByKey =
        destination.getPaymentOptionMetadata().stream()
            .collect(Collectors.toMap(PaymentOptionMetadata::getKey, po -> po));

    List<PaymentOptionMetadataModel> sourcePaymentOptionMetadata =
        source.getPaymentOptionMetadata();
    List<PaymentOptionMetadata> metadataToRemove =
        new ArrayList<>(destination.getPaymentOptionMetadata());

    if (sourcePaymentOptionMetadata != null) {
      for (PaymentOptionMetadataModel sourceMetadata : sourcePaymentOptionMetadata) {
        PaymentOptionMetadata managedMetadata =
            managedPaymentOptionMetadataByKey.get(sourceMetadata.getKey());

        if (managedMetadata != null) {
          // UPDATE
          sourceMetadata.setValue(managedMetadata.getValue());
          metadataToRemove.remove(managedMetadata);
        } else {
          // CREATE
          PaymentOptionMetadata md =
              PaymentOptionMetadata.builder()
                  .key(sourceMetadata.getKey())
                  .value(sourceMetadata.getValue())
                  .paymentOption(destination)
                  .build();
          destination.getPaymentOptionMetadata().add(md);
        }
      }
    }
    // DELETE
    destination.getPaymentOptionMetadata().removeAll(metadataToRemove);
  }

  private void mapAndUpdateTransferMetadata(TransferModel source, Transfer destination) {
    Map<String, TransferMetadata> managedTransferMetadataByKey =
        destination.getTransferMetadata().stream()
            .collect(Collectors.toMap(TransferMetadata::getKey, po -> po));

    List<TransferMetadataModel> sourceTransferMetadata = source.getTransferMetadata();
    List<TransferMetadata> metadataToRemove = new ArrayList<>(destination.getTransferMetadata());

    if (sourceTransferMetadata != null) {
      for (TransferMetadataModel sourceMetadata : sourceTransferMetadata) {
        TransferMetadata managedMetadata =
            managedTransferMetadataByKey.get(sourceMetadata.getKey());

        if (managedMetadata != null) {
          // UPDATE
          sourceMetadata.setValue(managedMetadata.getValue());
          metadataToRemove.remove(managedMetadata);
        } else {
          // CREATE
          TransferMetadata md =
              TransferMetadata.builder()
                  .key(sourceMetadata.getKey())
                  .value(sourceMetadata.getValue())
                  .transfer(destination)
                  .build();
          destination.getTransferMetadata().add(md);
        }
      }
    }
    // DELETE
    destination.getTransferMetadata().removeAll(metadataToRemove);
  }
  
  private Optional<String> findExistingSharedPlanUuid(
		  java.util.Collection<PaymentOption> managedOptions) {
	  for (PaymentOption existing : managedOptions) {
		  if (Boolean.TRUE.equals(existing.getIsPartialPayment())) {
			  String pid = existing.getPaymentPlanId();
			  if (pid != null && isUuid(pid)) {
				  return Optional.of(pid);
			  }
		  }
	  }
	  return Optional.empty();
  }

  private boolean isUuid(String s) {
	  try {
		  java.util.UUID.fromString(s);
		  return true;
	  } catch (IllegalArgumentException e) {
		  return false;
	  }
  }
}
