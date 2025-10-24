package it.gov.pagopa.debtposition.repository;

import it.gov.pagopa.debtposition.entity.PaymentOption;
import it.gov.pagopa.debtposition.entity.PaymentPosition;
import it.gov.pagopa.debtposition.model.enumeration.PaymentOptionStatus;
import java.util.List;
import java.util.Optional;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.stereotype.Repository;

/**
 * @author aacitelli
 */
@Repository
public interface PaymentOptionRepository
    extends JpaRepository<PaymentOption, Long>, JpaSpecificationExecutor<PaymentOption> { }
