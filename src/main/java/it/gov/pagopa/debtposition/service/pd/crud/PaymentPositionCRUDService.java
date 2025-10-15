package it.gov.pagopa.debtposition.service.pd.crud;

import static it.gov.pagopa.debtposition.service.payments.PaymentsService.findPrimaryTransfer;
import static it.gov.pagopa.debtposition.util.Constants.NOTIFICATION_FEE_METADATA_KEY;
import static org.springframework.data.jpa.domain.Specification.allOf;

import it.gov.pagopa.debtposition.entity.Installment;
import it.gov.pagopa.debtposition.entity.InstallmentMetadata;
import it.gov.pagopa.debtposition.entity.PaymentPosition;
import it.gov.pagopa.debtposition.entity.Transfer;
import it.gov.pagopa.debtposition.exception.AppError;
import it.gov.pagopa.debtposition.exception.AppException;
import it.gov.pagopa.debtposition.exception.ValidationException;
import it.gov.pagopa.debtposition.model.IPaymentPositionModel;
import it.gov.pagopa.debtposition.model.enumeration.DebtPositionStatus;
import it.gov.pagopa.debtposition.model.enumeration.PaymentOptionStatus;
import it.gov.pagopa.debtposition.model.enumeration.TransferStatus;
import it.gov.pagopa.debtposition.model.filterandorder.FilterAndOrder;
import it.gov.pagopa.debtposition.model.pd.PaymentPositionModel;
import it.gov.pagopa.debtposition.repository.PaymentOptionRepository;
import it.gov.pagopa.debtposition.repository.PaymentPositionRepository;
import it.gov.pagopa.debtposition.repository.TransferRepository;
import it.gov.pagopa.debtposition.repository.specification.*;
import it.gov.pagopa.debtposition.util.CommonUtil;
import it.gov.pagopa.debtposition.util.DebtPositionValidation;
import it.gov.pagopa.debtposition.util.PublishPaymentUtil;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.util.*;
import jakarta.validation.Valid;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Positive;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.SerializationUtils;
import org.hibernate.exception.ConstraintViolationException;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.dao.DataIntegrityViolationException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@Slf4j
public class PaymentPositionCRUDService {

  private static final String UNIQUE_KEY_VIOLATION = "23505";

  @Value("${max.days.interval}")
  private String maxDaysInterval;

  @Autowired private PaymentPositionRepository paymentPositionRepository;
  @Autowired private PaymentOptionRepository paymentOptionRepository;
  @Autowired private ModelMapper modelMapper;
  @Autowired private TransferRepository transferRepository;

  @Value("${nav.aux.digit}")
  private String auxDigit;

  public PaymentPosition create(
      @NotNull PaymentPosition debtPosition,
      @NotBlank String organizationFiscalCode,
      boolean toPublish,
      List<String> segCodes,
      String... action) {

    final String ERROR_CREATION_LOG_MSG = "Error during debt position creation: %s";

    try {
    	// Inserts (and possibly brings to published status) the debt position
    	PaymentPosition toSave =
    	        this.checkAndBuildDebtPositionToSave(
    	            debtPosition, organizationFiscalCode, toPublish, segCodes, action);
    	assignPaymentPlanIds(toSave);
    	
    	return paymentPositionRepository.saveAndFlush(toSave);
    } catch (DataIntegrityViolationException e) {
      log.error(String.format(ERROR_CREATION_LOG_MSG, e.getMessage()), e);
      if (e.getCause() instanceof ConstraintViolationException constraintviolationexception) {
        String sqlState = constraintviolationexception.getSQLState();
        if (sqlState.equals(UNIQUE_KEY_VIOLATION)) {
          throw new AppException(AppError.DEBT_POSITION_UNIQUE_VIOLATION, organizationFiscalCode);
        }
      }
      throw new AppException(AppError.DEBT_POSITION_CREATION_FAILED, organizationFiscalCode);
    } catch (ValidationException e) {
      throw new AppException(AppError.DEBT_POSITION_REQUEST_DATA_ERROR, e.getMessage());
    } catch (AppException appException) {
      throw appException;
    } catch (Exception e) {
      log.error(String.format(ERROR_CREATION_LOG_MSG, e.getMessage()), e);
      throw new AppException(AppError.DEBT_POSITION_CREATION_FAILED, organizationFiscalCode);
    }
  }

  public PaymentPosition getDebtPositionByIUPD(
      String organizationFiscalCode, String iupd, List<String> segCodes) {

    Specification<PaymentPosition> spec =
    		allOf(
            new PaymentPositionByOrganizationFiscalCode(organizationFiscalCode)
                .and(new PaymentPositionByIUPD(iupd)));

    Optional<PaymentPosition> ppOptional = paymentPositionRepository.findOne(spec);
    if (ppOptional.isEmpty()) {
      throw new AppException(AppError.DEBT_POSITION_NOT_FOUND, organizationFiscalCode, iupd);
    }
    
    PaymentPosition pp = ppOptional.get();
    
    if (segCodes != null && !isAuthorizedBySegregationCode(pp, segCodes)) {
      throw new AppException(AppError.DEBT_POSITION_FORBIDDEN, organizationFiscalCode, iupd);
    }

    return pp;
  }

  public PaymentPosition getDebtPositionByIUV(
      String organizationFiscalCode, String iuv, List<String> segCodes) {
    if (segCodes != null && !isAuthorizedBySegregationCode(iuv, segCodes)) {
      throw new AppException(AppError.DEBT_POSITION_FORBIDDEN, organizationFiscalCode, iuv);
    }

    Optional<Installment> po =
        paymentOptionRepository.findByOrganizationFiscalCodeAndIuv(organizationFiscalCode, iuv);

    if (po.isEmpty()) {
      throw new AppException(AppError.PAYMENT_OPTION_IUV_NOT_FOUND, organizationFiscalCode, iuv);
    }

    Installment inst = po.get();
    PaymentPosition pp = inst.getPaymentPosition();

    return pp;
  }

  public List<PaymentPosition> getDebtPositionsByIUPDs(
      String organizationFiscalCode, List<String> iupdList, List<String> segCodes) {
    // findAll query by IUPD list
    Specification<PaymentPosition> spec =
    		allOf(new PaymentPositionByOrganizationFiscalCode(organizationFiscalCode)
                            .and(new PaymentPositionByIUPDList(iupdList)));

    Pageable pageable = PageRequest.of(0, iupdList.size());
    Page<PaymentPosition> result = paymentPositionRepository.findAll(spec, pageable);
    List<PaymentPosition> paymentPositions = result.getContent();

    if (paymentPositions.isEmpty() || paymentPositions.size() != iupdList.size()) {
      throw new AppException(AppError.DEBT_POSITION_NOT_FOUND, organizationFiscalCode, iupdList);
    }

    // verify that caller is authorized to read payment positions
    paymentPositions.forEach(
        pp -> {
          if (segCodes != null && !isAuthorizedBySegregationCode(pp, segCodes)) {
            throw new AppException(
                AppError.DEBT_POSITION_FORBIDDEN, organizationFiscalCode, pp.getIupd());
          }
        });

    return paymentPositions;
  }

  public Page<PaymentPosition> getOrganizationDebtPositions(
      @Positive Integer limit, @Positive Integer pageNum, FilterAndOrder filterAndOrder) {

    checkAndUpdateDates(filterAndOrder);

    Pageable pageable = PageRequest.of(pageNum, limit, CommonUtil.getSort(filterAndOrder));

    Specification<PaymentPosition> paymentPositionSpecification =
        new PaymentPositionByOrganizationFiscalCode(
                filterAndOrder.getFilter().getOrganizationFiscalCode())
            .and(
                new PaymentPositionByOptionsAttribute(
                        filterAndOrder.getFilter().getDueDateFrom(),
                        filterAndOrder.getFilter().getDueDateTo(),
                        filterAndOrder.getFilter().getSegregationCodes())
                    .and(
                        new PaymentPositionByPaymentDate(
                            filterAndOrder.getFilter().getPaymentDateFrom(),
                            filterAndOrder.getFilter().getPaymentDateTo()))
                    .and(new PaymentPositionByStatus(filterAndOrder.getFilter().getStatus())));

    Specification<PaymentPosition> specPP = allOf(paymentPositionSpecification);

    Page<PaymentPosition> page = paymentPositionRepository.findAll(specPP, pageable);
    List<PaymentPosition> positions = page.getContent();
    // The retrieval of PaymentOptions is done manually to apply filters which, in the automatic
    // fetch, are not used by JPA
    for (PaymentPosition pp : positions) {
      Specification<Installment> specPO =
    		  allOf(
              new PaymentOptionByAttribute(
                  pp,
                  filterAndOrder.getFilter().getDueDateFrom(),
                  filterAndOrder.getFilter().getDueDateTo(),
                  filterAndOrder.getFilter().getSegregationCodes()));

      List<Installment> poList = paymentOptionRepository.findAll(specPO);
      pp.setPaymentOption(poList);
      
    }

    return CommonUtil.toPage(positions, page.getNumber(), page.getSize(), page.getTotalElements());
  }

  @Transactional
  public void delete(
      @NotBlank String organizationFiscalCode,
      @NotBlank String iupd,
      List<String> segregationCodes) {
    PaymentPosition ppToRemove =
        this.getDebtPositionByIUPD(organizationFiscalCode, iupd, segregationCodes);
    if (DebtPositionStatus.getPaymentPosAlreadyPaidStatus().contains(ppToRemove.getStatus())) {
      throw new AppException(AppError.DEBT_POSITION_PAYMENT_FOUND, organizationFiscalCode, iupd);
    }
    paymentPositionRepository.delete(ppToRemove);
  }

  @Transactional
  public PaymentPosition update(
      @NotNull @Valid IPaymentPositionModel ppModel,
      @NotBlank String organizationFiscalCode,
      boolean toPublish,
      List<String> segregationCodes,
      String... action) {

    final String ERROR_UPDATE_LOG_MSG = "Error during debt position update: %s";

    PaymentPosition ppToUpdate =
        this.getDebtPositionByIUPD(organizationFiscalCode, ppModel.getIupd(), segregationCodes);

    if (DebtPositionStatus.getPaymentPosNotUpdatableStatus().contains(ppToUpdate.getStatus())) {
      throw new AppException(
          AppError.DEBT_POSITION_NOT_UPDATABLE, organizationFiscalCode, ppModel.getIupd());
    }

    try {

      // flip model to entity
      modelMapper.map(ppModel, ppToUpdate);
      
      // assign/reuse paymentPlanIds
      assignPaymentPlanIds(ppToUpdate);

      // update amounts adding notification fee
      updateAmounts(organizationFiscalCode, ppToUpdate);

      // check the data
      DebtPositionValidation.checkPaymentPositionInputDataAccuracy(ppToUpdate, action);

      var ppUpdated =
          this.checkDebtPositionToUpdate(
              ppToUpdate, organizationFiscalCode, toPublish, segregationCodes, action);
      
      return paymentPositionRepository.saveAndFlush(ppUpdated);

    } catch (ValidationException e) {
      throw new AppException(AppError.DEBT_POSITION_REQUEST_DATA_ERROR, e.getMessage());
    } catch (AppException e) {
      if (AppError.PAYMENT_OPTION_NOTIFICATION_FEE_UPDATE_TRANSFER_NOT_FOUND.title.equals(
          e.getTitle())) {
        throw new AppException(
            AppError.DEBT_POSITION_UPDATE_FAILED_NO_TRANSFER_FOR_NOTIFICATION_FEE,
            organizationFiscalCode,
            ppToUpdate.getIupd());
      }
      throw e;
    } catch (Exception e) {
      log.error(String.format(ERROR_UPDATE_LOG_MSG, e.getMessage()), e);
      throw new AppException(AppError.DEBT_POSITION_UPDATE_FAILED, organizationFiscalCode);
    }
  }

  /**
   * This method adds the notification fee to the amount in the PaymentOption and the Primary
   * Transfer
   *
   * @param organizationFiscalCode EC
   * @param ppToUpdate the entity of the debt position to update
   */
  private static void updateAmounts(String organizationFiscalCode, PaymentPosition ppToUpdate) {
    ppToUpdate
        .getPaymentOption()
        .forEach(
            po -> {
              if (po.getNotificationFee() > 0) {
                // update amount in the PaymentOption
                po.setAmount(po.getAmount() + po.getNotificationFee());

                if (!po.getTransfer().isEmpty()) {
                  // update amount in the Transfer
                  Transfer primaryTransfer = findPrimaryTransfer(po, organizationFiscalCode);
                  primaryTransfer.setAmount(primaryTransfer.getAmount() + po.getNotificationFee());
                }
              }
            });
  }

  public List<PaymentPosition> createMultipleDebtPositions(
      @Valid List<PaymentPosition> debtPositions,
      String organizationFiscalCode,
      boolean toPublish,
      List<String> segCodes,
      String... action) {

    final String ERROR_CREATION_LOG_MSG = "Error during debt position creation: %s";

    List<PaymentPosition> ppToSaveList = new ArrayList<>();

    try {

     for (PaymentPosition debtPosition : debtPositions) {
    		PaymentPosition pp = this.checkDebtPositionToUpdate(
    				debtPosition, organizationFiscalCode, toPublish, segCodes, action);
    		assignPaymentPlanIds(pp);
    		
    		ppToSaveList.add(pp);
     }

      // Inserisce il blocco di posizioni debitorie
      return paymentPositionRepository.saveAllAndFlush(ppToSaveList);

    } catch (DataIntegrityViolationException e) {
      log.error(String.format(ERROR_CREATION_LOG_MSG, e.getMessage()), e);
      if (e.getCause() instanceof ConstraintViolationException constraintviolationexception) {
        String sqlState = constraintviolationexception.getSQLState();
        if (sqlState.equals(UNIQUE_KEY_VIOLATION)) {
          throw new AppException(AppError.DEBT_POSITION_UNIQUE_VIOLATION, organizationFiscalCode);
        }
      }
      throw new AppException(AppError.DEBT_POSITION_CREATION_FAILED, organizationFiscalCode);
    } catch (ValidationException e) {
      throw new AppException(AppError.DEBT_POSITION_REQUEST_DATA_ERROR, e.getMessage());
    } catch (AppException appException) {
      throw appException;
    } catch (Exception e) {
      log.error(String.format(ERROR_CREATION_LOG_MSG, e.getMessage()), e);
      throw new AppException(AppError.DEBT_POSITION_CREATION_FAILED, organizationFiscalCode);
    }
  }

  @Transactional
  public List<PaymentPosition> updateMultipleDebtPositions(
      @Valid List<PaymentPositionModel> inputPaymentPositions,
      String organizationFiscalCode,
      boolean toPublish,
      List<String> segCodes,
      String... action) {

    Map<String, PaymentPositionModel> inPositionsMap = new HashMap<>();
    inputPaymentPositions.forEach(pp -> inPositionsMap.put(pp.getIupd(), pp));

    List<PaymentPosition> managedPositions =
        getDebtPositionsByIUPDs(
            organizationFiscalCode, inPositionsMap.keySet().stream().toList(), segCodes);

    try {
      for (PaymentPosition currentPP : managedPositions) {
        PaymentPositionModel inputPaymentPosition = inPositionsMap.get(currentPP.getIupd());

        if (DebtPositionStatus.getPaymentPosNotUpdatableStatus().contains(currentPP.getStatus())) {
          throw new AppException(
              AppError.DEBT_POSITION_NOT_UPDATABLE,
              organizationFiscalCode,
              inputPaymentPosition.getIupd());
        }

        // map model to entity
        modelMapper.map(inputPaymentPosition, currentPP);
        
        // assign/reuse paymentPlanIds
        assignPaymentPlanIds(currentPP);

        // update amounts adding notification fee
        updateAmounts(organizationFiscalCode, currentPP);
        
        // check data
        DebtPositionValidation.checkPaymentPositionInputDataAccuracy(currentPP, action);
      }
      this.createMultipleDebtPositions(
          managedPositions, organizationFiscalCode, toPublish, segCodes, action);

      return managedPositions;

    } catch (ValidationException e) {
      throw new AppException(AppError.DEBT_POSITION_REQUEST_DATA_ERROR, e.getMessage());
    } catch (AppException e) {
      if (AppError.PAYMENT_OPTION_NOTIFICATION_FEE_UPDATE_TRANSFER_NOT_FOUND.title.equals(
          e.getTitle())) {
        throw new AppException(
            AppError.DEBT_POSITION_UPDATE_FAILED_NO_TRANSFER_FOR_NOTIFICATION_FEE,
            organizationFiscalCode,
            e.getMessage());
      }
      throw e;
    } catch (Exception e) {
      // Log the entire exception for debugging purposes.
      log.error("Error during debt position update process", e);
      throw new AppException(AppError.DEBT_POSITION_UPDATE_FAILED, organizationFiscalCode);
    }
  }

  @Transactional
  public void deleteMultipleDebtPositions(
      @Valid List<String> multipleIUPDs, String organizationFiscalCode, List<String> segCodes) {
    final String ERROR_UPDATE_LOG_MSG = "Error during debt positions delete: %s";

    List<PaymentPosition> readPositions =
        getDebtPositionsByIUPDs(organizationFiscalCode, multipleIUPDs, segCodes);

    readPositions.forEach(
        ppToRemove -> {
          if (DebtPositionStatus.getPaymentPosAlreadyPaidStatus()
              .contains(ppToRemove.getStatus())) {
            throw new AppException(
                AppError.DEBT_POSITION_PAYMENT_FOUND, organizationFiscalCode, ppToRemove.getIupd());
          }
        });

    try {
      paymentPositionRepository.deleteAll(readPositions);
      paymentPositionRepository.flush();
    } catch (AppException e) {
      throw e;
    } catch (Exception e) {
      log.error(String.format(ERROR_UPDATE_LOG_MSG, e.getMessage()), e);
      throw new AppException(AppError.DEBT_POSITION_DELETE_FAILED, organizationFiscalCode);
    }
  }

  /*
   * Checks whether the date interval submitted is appropriate. Dates are updated by adding or subtracting the maxDaysInterval
   * in cases where one and only one of the two bounds of the date interval [from, to] is missing.
   */
  public void checkAndUpdateDates(FilterAndOrder filterAndOrder) {
    List<LocalDateTime> verifiedDueDates =
        DebtPositionValidation.checkDatesInterval(
            filterAndOrder.getFilter().getDueDateFrom(),
            filterAndOrder.getFilter().getDueDateTo(),
            Integer.parseInt(maxDaysInterval));
    List<LocalDateTime> verifiedPaymentDates =
        DebtPositionValidation.checkDatesInterval(
            filterAndOrder.getFilter().getPaymentDateFrom(),
            filterAndOrder.getFilter().getPaymentDateTo(),
            Integer.parseInt(maxDaysInterval));

    filterAndOrder.getFilter().setDueDateFrom(verifiedDueDates.get(0));
    filterAndOrder.getFilter().setDueDateTo(verifiedDueDates.get(1));
    filterAndOrder.getFilter().setPaymentDateFrom(verifiedPaymentDates.get(0));
    filterAndOrder.getFilter().setPaymentDateTo(verifiedPaymentDates.get(1));
  }

  // Update all Organization's IBANs on Transfer of payable PaymentPosition
  @Transactional
  public int updateTransferIbanMassive(
      String organizationFiscalCode, String oldIban, String newIban, int limit) {
    int numberOfUpdates = 0;

    // Update all Transfers that have the specified payment_option_id and oldIban as IBAN
    numberOfUpdates +=
        transferRepository.updateTransferIban(
            organizationFiscalCode,
            oldIban,
            newIban,
            LocalDateTime.now(ZoneOffset.UTC),
            List.of(PaymentOptionStatus.PO_UNPAID.name()),
            List.of(
                DebtPositionStatus.DRAFT.name(),
                DebtPositionStatus.PUBLISHED.name(),
                DebtPositionStatus.VALID.name(),
                DebtPositionStatus.PARTIALLY_PAID.name()),
            limit);

    return numberOfUpdates;
  }

  private PaymentPosition checkDebtPositionToUpdate(
      PaymentPosition pp,
      String organizationFiscalCode,
      boolean toPublish,
      List<String> segCodes,
      String... action) {

    if (segCodes != null && !isAuthorizedBySegregationCode(pp, segCodes)) {
      throw new AppException(
          AppError.DEBT_POSITION_FORBIDDEN, organizationFiscalCode, pp.getIupd());
    }

    // verifico la correttezza dei dati in input
    DebtPositionValidation.checkPaymentPositionInputDataAccuracy(pp, action);

    // predispongo i dati ad uso interno prima dell'aggiornamento
    LocalDateTime currentDate = LocalDateTime.now(ZoneOffset.UTC);
    LocalDateTime minDueDate =
        pp.getPaymentOption().stream()
            .map(Installment::getDueDate)
            .min(LocalDateTime::compareTo)
            .orElse(currentDate);
    LocalDateTime maxDueDate =
        pp.getPaymentOption().stream()
            .map(Installment::getDueDate)
            .max(LocalDateTime::compareTo)
            .orElse(currentDate);
    pp.setMinDueDate(minDueDate);
    pp.setMaxDueDate(maxDueDate);
    pp.setInsertedDate(Objects.requireNonNullElse(pp.getInsertedDate(), currentDate));
    pp.setLastUpdatedDate(currentDate);
    pp.setPublishDate(null);
    pp.setOrganizationFiscalCode(organizationFiscalCode);
    pp.setStatus(DebtPositionStatus.DRAFT);
    pp.setServiceType(pp.getServiceType());

    for (Installment po : pp.getPaymentOption()) {
      // Make sure there isn't reserved metadata
      for (InstallmentMetadata pom : po.getPaymentOptionMetadata()) {
        if (pom.getKey().equals(NOTIFICATION_FEE_METADATA_KEY)) {
          throw new AppException(
              AppError.PAYMENT_OPTION_RESERVED_METADATA, organizationFiscalCode, pp.getIupd());
        }
      }

      po.setOrganizationFiscalCode(organizationFiscalCode);
      po.setInsertedDate(Objects.requireNonNullElse(pp.getInsertedDate(), currentDate));
      po.setLastUpdatedDate(currentDate);
      po.setStatus(PaymentOptionStatus.PO_UNPAID);
      po.setPaymentPosition(pp);
      po.getPaymentOptionMetadata().forEach(pom -> pom.setInstallment(po));
      po.setNav(Optional.ofNullable(po.getNav()).orElse(auxDigit + po.getIuv()));

      for (Transfer t : po.getTransfer()) {
        t.setIuv(po.getIuv());
        t.setOrganizationFiscalCode(
            Objects.requireNonNullElse(t.getOrganizationFiscalCode(), organizationFiscalCode));
        t.setCompanyName(t.getCompanyName());
        t.setInsertedDate(Objects.requireNonNullElse(pp.getInsertedDate(), currentDate));
        t.setLastUpdatedDate(currentDate);
        t.setStatus(TransferStatus.T_UNREPORTED);
        t.setPaymentOption(po);
        t.getTransferMetadata().forEach(tm -> tm.setTransfer(t));
      }
    }

    // Se la pubblicazione immediata è richiesta, si procede
    if (toPublish) {
      PublishPaymentUtil.publishProcess(pp, action);
    }

    return pp;
  }

  private PaymentPosition checkAndBuildDebtPositionToSave(
      PaymentPosition debtPosition,
      String organizationFiscalCode,
      boolean toPublish,
      List<String> segCodes,
      String... action) {

    PaymentPosition pp = SerializationUtils.clone(debtPosition);
    return checkDebtPositionToUpdate(pp, organizationFiscalCode, toPublish, segCodes, action);
  }

  private boolean isAuthorizedBySegregationCode(
      PaymentPosition paymentPosition, List<String> segregationCodes) {
    // It is enough to check only one IUV of the payment position. Here it is assumed that they all
    // have the same segregation code.
    String paymentPositionSegregationCode =
        paymentPosition.getPaymentOption().get(0).getIuv().substring(0, 2);
    return segregationCodes.contains(paymentPositionSegregationCode);
  }

  private boolean isAuthorizedBySegregationCode(String iuv, List<String> segregationCodes) {
    // It is enough to check only one IUV of the payment position. Here it is assumed that they all
    // have the same segregation code.
    String paymentPositionSegregationCode = iuv.substring(0, 2);
    return segregationCodes.contains(paymentPositionSegregationCode);
  }
  
  /*
   * TODO: Multi-plan (future)
   * ----------------------------------------------------------------------------
   * Context
   *  - Today we assume at most ONE installment plan per PaymentPosition.
   *  - assignPaymentPlanIds(...) sets a single plain UUID (no prefix) on all installments
   *    (isPartialPayment=true) and NULL on each single option (isPartialPayment=false).
   *
   * What we’ll need to support MULTIPLE installment plans within the SAME PaymentPosition:
   *
   * 1) Introduce in the payload a stable “plan key” per installment (for example):
   *    - leaderNav: each installment of a plan carries the nav of its plan leader
   *    - planIdx (1..N per PaymentPosition)
   *
   * 2) Change assignPaymentPlanIds(...) to:
   *    - Group installments by the chosen plan key.
   *    - For each group: reuse an existing plain UUID if present (idempotency), otherwise generate a new one.
   *    - Set that UUID on all installments of the group.
   *    - Keep singles with NULL payment_plan_id.
   *
   * Summary
   *  - Add/receive a plan key (leaderNav or planIdx).
   *  - Group by that key and assign one UUID per group.
   *  - Keep singles as NULL.
   */
	private void assignPaymentPlanIds(PaymentPosition pp) {
		if (pp.getPaymentOption() == null || pp.getPaymentOption().isEmpty())
			return;

		List<Installment> singles = new ArrayList<>();
		List<Installment> installments = new ArrayList<>();
		for (Installment i : pp.getPaymentOption()) {
			if (Boolean.TRUE.equals(i.getIsPartialPayment()))
				installments.add(i);
			else
				singles.add(i);
		}

		// Singles → force NULL
		for (Installment i : singles) {
			if (i.getPaymentPlanId() != null)
				i.setPaymentPlanId(null);
		}

		// Installments → a single shared UUID (no prefix) for the whole PaymentPosition
		// (today)
		if (!installments.isEmpty()) {
			String planId = existingInstallmentPlanUuid(installments).orElse(java.util.UUID.randomUUID().toString());
			for (Installment i : installments) {
				if (!planId.equals(i.getPaymentPlanId()))
					i.setPaymentPlanId(planId);
			}
		}
	}

	// Reuse only a valid plain UUID
	private Optional<String> existingInstallmentPlanUuid(List<Installment> installments) {
		return installments.stream().map(Installment::getPaymentPlanId).filter(Objects::nonNull).filter(this::isUuid)
				.findFirst();
	}

	// Accept canonical UUID format (case-insensitive)
	private boolean isUuid(String s) {
		try {java.util.UUID.fromString(s); return true;} 
		catch (IllegalArgumentException e) {return false;}
	}
}
