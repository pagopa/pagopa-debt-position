package it.gov.pagopa.debtposition.service.pd.crud;

import it.gov.pagopa.debtposition.entity.Installment;
import it.gov.pagopa.debtposition.entity.PaymentOption;
import it.gov.pagopa.debtposition.entity.PaymentPosition;
import it.gov.pagopa.debtposition.entity.Transfer;
import it.gov.pagopa.debtposition.exception.AppError;
import it.gov.pagopa.debtposition.exception.AppException;
import it.gov.pagopa.debtposition.exception.ValidationException;
import it.gov.pagopa.debtposition.model.IPaymentPositionModel;
import it.gov.pagopa.debtposition.model.enumeration.DebtPositionStatus;
import it.gov.pagopa.debtposition.model.enumeration.InstallmentStatus;
import it.gov.pagopa.debtposition.model.enumeration.TransferStatus;
import it.gov.pagopa.debtposition.model.filterandorder.FilterAndOrder;
import it.gov.pagopa.debtposition.model.pd.PaymentPositionModel;
import it.gov.pagopa.debtposition.repository.InstallmentRepository;
import it.gov.pagopa.debtposition.repository.PaymentPositionRepository;
import it.gov.pagopa.debtposition.repository.TransferRepository;
import it.gov.pagopa.debtposition.repository.specification.*;
import it.gov.pagopa.debtposition.util.CommonUtil;
import it.gov.pagopa.debtposition.util.DebtPositionValidation;
import it.gov.pagopa.debtposition.util.ObjectMapperUtils;
import it.gov.pagopa.debtposition.util.PublishPaymentUtil;
import jakarta.validation.Valid;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Positive;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.SerializationUtils;
import org.hibernate.exception.ConstraintViolationException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.dao.DataIntegrityViolationException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.util.*;

import static it.gov.pagopa.debtposition.service.payments.PaymentsService.findPrimaryTransfer;
import static org.springframework.data.jpa.domain.Specification.allOf;

@Service
@Slf4j
public class PaymentPositionCRUDService {
    private final InstallmentRepository installmentRepository;
    private static final String UNIQUE_KEY_VIOLATION = "23505";

    @Value("${max.days.interval}")
    private String maxDaysInterval;

    private final PaymentPositionRepository paymentPositionRepository;
    private final TransferRepository transferRepository;

    @Value("${nav.aux.digit}")
    private String auxDigit;

    @Autowired
    public PaymentPositionCRUDService(PaymentPositionRepository paymentPositionRepository, TransferRepository transferRepository,
                                      InstallmentRepository installmentRepository) {
        this.paymentPositionRepository = paymentPositionRepository;
        this.transferRepository = transferRepository;
        this.installmentRepository = installmentRepository;
    }

    public PaymentPosition create(
            @NotNull PaymentPosition debtPosition,
            @NotBlank String organizationFiscalCode,
            boolean toPublish,
            List<String> segCodes,
            String... action) {

        final String ERROR_CREATION_LOG_MSG = "Error during debt position creation: %s";

        try {
            // Inserisce (ed eventualmente porta in stato pubblicato) la posizione debitoria
            return paymentPositionRepository.saveAndFlush(
                    this.checkAndBuildDebtPositionToSave(
                            debtPosition, organizationFiscalCode, toPublish, segCodes, action));
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
                allOf(new PaymentPositionByOrganizationFiscalCode(organizationFiscalCode)
                        .and(new PaymentPositionByIUPD(iupd)));

        Optional<PaymentPosition> pp = paymentPositionRepository.findOne(spec);
        if (pp.isEmpty()) {
            throw new AppException(AppError.DEBT_POSITION_NOT_FOUND, organizationFiscalCode, iupd);
        }
        if (segCodes != null && !isAuthorizedBySegregationCode(pp.get(), segCodes)) {
            throw new AppException(AppError.DEBT_POSITION_FORBIDDEN, organizationFiscalCode, iupd);
        }

        return pp.get();
    }

    public PaymentPosition getDebtPositionByIUV(
            String organizationFiscalCode, String iuv, List<String> segCodes) {
        if (segCodes != null && !isAuthorizedBySegregationCode(iuv, segCodes)) {
            throw new AppException(AppError.DEBT_POSITION_FORBIDDEN, organizationFiscalCode, iuv);
        }

        Optional<PaymentPosition> pp =
                paymentPositionRepository.findByPaymentOptionOrganizationFiscalCodeAndPaymentOptionInstallmentIuv(organizationFiscalCode, iuv);

        if (pp.isEmpty()) { // TODO change error
            throw new AppException(AppError.PAYMENT_OPTION_IUV_NOT_FOUND, organizationFiscalCode, iuv);
        }

        return pp.get();
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
            List<PaymentOption> poToAdd = new ArrayList<>();
            for(PaymentOption po : pp.getPaymentOption()){
                Specification<Installment> specInst =
                        allOf(
                                new InstallmentByAttribute(
                                        po,
                                        filterAndOrder.getFilter().getDueDateFrom(),
                                        filterAndOrder.getFilter().getDueDateTo(),
                                        filterAndOrder.getFilter().getSegregationCodes()));

                List<Installment> instList = installmentRepository.findAll(specInst);

                if(!instList.isEmpty()){
                    po.setInstallment(instList);
                    poToAdd.add(po);
                }
            }
            pp.setPaymentOption(poToAdd);
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
            ObjectMapperUtils.map(ppModel, ppToUpdate);

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
     * @param ppToUpdate             the entity of the debt position to update
     */
    private static void updateAmounts(String organizationFiscalCode, PaymentPosition ppToUpdate) {
        ppToUpdate
                .getPaymentOption()
                .forEach(
                        po ->
                                po.getInstallment().forEach(inst -> {
                                    if (inst.getNotificationFee() > 0) {
                                        // update amount in the PaymentOption
                                        inst.setAmount(inst.getAmount() + inst.getNotificationFee());

                                        if (!inst.getTransfer().isEmpty()) {
                                            // update amount in the Transfer
                                            Transfer primaryTransfer = findPrimaryTransfer(inst, organizationFiscalCode);
                                            primaryTransfer.setAmount(primaryTransfer.getAmount() + inst.getNotificationFee());
                                        }
                                    }
                                })
                );
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
                ppToSaveList.add(
                        this.checkDebtPositionToUpdate(
                                debtPosition, organizationFiscalCode, toPublish, segCodes, action));
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
                ObjectMapperUtils.map(inputPaymentPosition, currentPP);

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
                        List.of(InstallmentStatus.UNPAID.name(), InstallmentStatus.DRAFT.name()),
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
        List<Installment> installmentList = pp.getPaymentOption().stream().map(PaymentOption::getInstallment).flatMap(List::stream).toList();
        LocalDateTime minDueDate =
                installmentList.stream()
                        .map(Installment::getDueDate)
                        .min(LocalDateTime::compareTo)
                        .orElse(currentDate);
        LocalDateTime maxDueDate =
                installmentList.stream()
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

        for (PaymentOption po : pp.getPaymentOption()) {
            // Make sure there isn't reserved metadata
            // TODO
//      for (PaymentOptionMetadata pom : po.getPaymentOptionMetadata()) {
//        if (pom.getKey().equals(NOTIFICATION_FEE_METADATA_KEY)) {
//          throw new AppException(
//              AppError.PAYMENT_OPTION_RESERVED_METADATA, organizationFiscalCode, pp.getIupd());
//        }
//      }
// TODO verify mapping
            for (Installment inst : po.getInstallment()) {
                inst.setOrganizationFiscalCode(organizationFiscalCode);
                inst.setInsertedDate(Objects.requireNonNullElse(pp.getInsertedDate(), currentDate));
                inst.setLastUpdatedDate(currentDate);
                inst.setStatus(InstallmentStatus.DRAFT);
                inst.setPaymentPosition(pp);
                // po.getPaymentOptionMetadata().forEach(pom -> pom.setPaymentOption(po));
                inst.setNav(Optional.ofNullable(inst.getNav()).orElse(auxDigit + inst.getIuv()));

                for (Transfer t : inst.getTransfer()) {
                    t.setIuv(inst.getIuv());
                    t.setOrganizationFiscalCode(
                            Objects.requireNonNullElse(t.getOrganizationFiscalCode(), organizationFiscalCode));
                    t.setInsertedDate(Objects.requireNonNullElse(pp.getInsertedDate(), currentDate));
                    t.setLastUpdatedDate(currentDate);
                    t.setStatus(TransferStatus.T_UNREPORTED);
                    t.setInstallment(inst);
                    // TODO t.getTransferMetadata().forEach(tm -> tm.setTransfer(t));
                }
                inst.setPaymentOption(po);
            }
            po.setOrganizationFiscalCode(organizationFiscalCode);
            po.setInsertedDate(Objects.requireNonNullElse(pp.getInsertedDate(), currentDate));
            po.setPaymentPosition(pp);
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
                paymentPosition.getPaymentOption().stream().map(PaymentOption::getInstallment).flatMap(List::stream).toList().get(0).getIuv().substring(0, 2);
        return segregationCodes.contains(paymentPositionSegregationCode);
    }

    private boolean isAuthorizedBySegregationCode(String iuv, List<String> segregationCodes) {
        // It is enough to check only one IUV of the payment position. Here it is assumed that they all
        // have the same segregation code.
        String paymentPositionSegregationCode = iuv.substring(0, 2);
        return segregationCodes.contains(paymentPositionSegregationCode);
    }
}
