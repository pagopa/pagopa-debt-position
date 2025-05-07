package it.gov.pagopa.debtposition.service.pd.crud;

import it.gov.pagopa.debtposition.entity.PaymentOption;
import it.gov.pagopa.debtposition.entity.PaymentOptionMetadata;
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
import it.gov.pagopa.debtposition.repository.PaymentOptionRepository;
import it.gov.pagopa.debtposition.repository.PaymentPositionRepository;
import it.gov.pagopa.debtposition.repository.TransferRepository;
import it.gov.pagopa.debtposition.repository.specification.*;
import it.gov.pagopa.debtposition.service.payments.PaymentsService;
import it.gov.pagopa.debtposition.util.CommonUtil;
import it.gov.pagopa.debtposition.util.DebtPositionValidation;
import it.gov.pagopa.debtposition.util.PublishPaymentUtil;
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

import javax.validation.Valid;
import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Positive;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.util.*;
import java.util.stream.Collectors;

import static it.gov.pagopa.debtposition.util.Constants.NOTIFICATION_FEE_METADATA_KEY;

@Service
@Slf4j
public class PaymentPositionCRUDService {

    private static final String UNIQUE_KEY_VIOLATION = "23505";

    @Value("${max.days.interval}")
    private String maxDaysInterval;

    @Autowired
    private PaymentPositionRepository paymentPositionRepository;
    @Autowired
    private PaymentOptionRepository paymentOptionRepository;
    @Autowired
    private ModelMapper modelMapper;
    @Autowired
    private TransferRepository transferRepository;

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
                Specification.where(
                        new PaymentPositionByOrganizationFiscalCode(organizationFiscalCode)
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

        Optional<PaymentOption> po =
                paymentOptionRepository.findByOrganizationFiscalCodeAndIuv(organizationFiscalCode, iuv);

        if (po.isEmpty()) {
            throw new AppException(AppError.PAYMENT_OPTION_IUV_NOT_FOUND, organizationFiscalCode, iuv);
        }

        return po.get().getPaymentPosition();
    }

    public List<PaymentPosition> getDebtPositionsByIUPDs(
            String organizationFiscalCode, List<String> iupdList, List<String> segCodes) {
        // findAll query by IUPD list
        PaymentPositionByIUPDList spec = new PaymentPositionByIUPDList(iupdList);
        Specification<PaymentPosition> specPP = Specification.where(spec);
        Pageable pageable = PageRequest.of(0, iupdList.size());
        Page<PaymentPosition> result = paymentPositionRepository.findAll(specPP, pageable);
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

        Specification<PaymentPosition> specPP = Specification.where(paymentPositionSpecification);

        Page<PaymentPosition> page = paymentPositionRepository.findAll(specPP, pageable);
        List<PaymentPosition> positions = page.getContent();
        // The retrieval of PaymentOptions is done manually to apply filters which, in the automatic
        // fetch, are not used by JPA
        for (PaymentPosition pp : positions) {
            Specification<PaymentOption> specPO =
                    Specification.where(
                            new PaymentOptionByAttribute(
                                    pp,
                                    filterAndOrder.getFilter().getDueDateFrom(),
                                    filterAndOrder.getFilter().getDueDateTo(),
                                    filterAndOrder.getFilter().getSegregationCodes()));

            List<PaymentOption> poList = paymentOptionRepository.findAll(specPO);
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
            List<PaymentOption> oldPaymentOptions = new ArrayList<>(ppToUpdate.getPaymentOption());
            ppToUpdate.getPaymentOption().clear();
            modelMapper.map(ppModel, ppToUpdate);

            // migrate the notification fee value (if defined) and update the amounts
            setOldNotificationFee(oldPaymentOptions, organizationFiscalCode, ppToUpdate);

            // check the input data
            DebtPositionValidation.checkPaymentPositionInputDataAccuracy(ppToUpdate, action);

            paymentPositionRepository.delete(ppToUpdate);
            paymentPositionRepository.flush();
            // the version is increased at each change
            ppToUpdate.setVersion(ppToUpdate.getVersion() + 1);

            return this.create(ppToUpdate, organizationFiscalCode, toPublish, segregationCodes, action);
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
                        this.checkAndBuildDebtPositionToSave(
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
            @Valid List<PaymentPosition> inputPaymentPositions,
            String organizationFiscalCode,
            boolean toPublish,
            List<String> segCodes,
            String... action) {
        final String ERROR_UPDATE_LOG_MSG = "Error during debt positions update: %s";
        List<PaymentPosition> updatePositions = new ArrayList<>();
        Map<String, PaymentPosition> inPositionsMap = new HashMap<>();
        inputPaymentPositions.forEach(pp -> inPositionsMap.put(pp.getIupd(), pp));
        List<PaymentPosition> currentPaymentPositions =
                getDebtPositionsByIUPDs(
                        organizationFiscalCode, inPositionsMap.keySet().stream().toList(), segCodes);

        try {
            for (PaymentPosition currentPP : currentPaymentPositions) {
                PaymentPosition inputPaymentPosition = inPositionsMap.get(currentPP.getIupd());
                PaymentPosition newPaymentPosition = currentPP.deepClone();

                if (DebtPositionStatus.getPaymentPosNotUpdatableStatus()
                        .contains(currentPP.getStatus())) {
                    throw new AppException(
                            AppError.DEBT_POSITION_NOT_UPDATABLE,
                            organizationFiscalCode,
                            inputPaymentPosition.getIupd());
                }

                // flip model to entity
                List<PaymentOption> oldPaymentOptions = new ArrayList<>(newPaymentPosition.getPaymentOption());
                newPaymentPosition.getPaymentOption().clear();
                modelMapper.map(inputPaymentPosition, newPaymentPosition);

                // migrate the notification fee value (if defined) and update the amounts
                newPaymentPosition =
                        setOldNotificationFee(
                                oldPaymentOptions,
                                organizationFiscalCode,
                                newPaymentPosition);

                // check the input data
                DebtPositionValidation.checkPaymentPositionInputDataAccuracy(newPaymentPosition, action);
                // the version is increased at each change
                newPaymentPosition.setVersion(newPaymentPosition.getVersion() + 1);

                updatePositions.add(newPaymentPosition);
            }

            paymentPositionRepository.deleteAll(currentPaymentPositions);
            paymentPositionRepository.flush();
            this.createMultipleDebtPositions(
                    updatePositions, organizationFiscalCode, toPublish, segCodes, action);

            return updatePositions;
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
            log.error(String.format(ERROR_UPDATE_LOG_MSG, e.getMessage()), e);
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

    private PaymentPosition setOldNotificationFee(
            List<PaymentOption> oldPaymentOptions,
            String organizationFiscalCode,
            PaymentPosition paymentPosition) {
        Map<String, Long> oldPONotificationFeeMapping =
                oldPaymentOptions.stream()
                        .collect(Collectors.toMap(PaymentOption::getIuv, PaymentOption::getNotificationFee));
        for (PaymentOption paymentOptionModel : paymentPosition.getPaymentOption()) {
            long oldNotificationFee =
                    Objects.requireNonNullElse(
                            oldPONotificationFeeMapping.get(paymentOptionModel.getIuv()), 0L);
            if (oldNotificationFee != 0) {
                PaymentsService.updateAmountsWithNotificationFee(
                        paymentOptionModel, organizationFiscalCode, oldNotificationFee);
            }
        }
        return paymentPosition;
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
                        List.of(DebtPositionStatus.DRAFT.name(), DebtPositionStatus.PUBLISHED.name(), DebtPositionStatus.VALID.name(), DebtPositionStatus.PARTIALLY_PAID.name()),
                        limit
                );

        return numberOfUpdates;
    }

    private PaymentPosition checkAndBuildDebtPositionToSave(
            PaymentPosition debtPosition,
            String organizationFiscalCode,
            boolean toPublish,
            List<String> segCodes,
            String... action) {

        PaymentPosition pp = SerializationUtils.clone(debtPosition);

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
                        .map(PaymentOption::getDueDate)
                        .min(LocalDateTime::compareTo)
                        .orElse(currentDate);
        LocalDateTime maxDueDate =
                pp.getPaymentOption().stream()
                        .map(PaymentOption::getDueDate)
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
            for (PaymentOptionMetadata pom : po.getPaymentOptionMetadata()) {
                if (pom.getKey().equals(NOTIFICATION_FEE_METADATA_KEY)) {
                    throw new AppException(AppError.PAYMENT_OPTION_RESERVED_METADATA, organizationFiscalCode, pp.getIupd());
                }
            }

            po.setOrganizationFiscalCode(organizationFiscalCode);
            po.setInsertedDate(Objects.requireNonNullElse(pp.getInsertedDate(), currentDate));
            po.setLastUpdatedDate(currentDate);
            po.setStatus(PaymentOptionStatus.PO_UNPAID);
            po.setPaymentPosition(pp);
            po.getPaymentOptionMetadata().forEach(pom -> pom.setPaymentOption(po));
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
}
