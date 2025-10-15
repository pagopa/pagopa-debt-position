package it.gov.pagopa.debtposition.repository;

import it.gov.pagopa.debtposition.entity.Installment;
import it.gov.pagopa.debtposition.entity.PaymentPosition;
import it.gov.pagopa.debtposition.model.enumeration.PaymentOptionStatus;
import java.util.List;
import java.util.Optional;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

/**
 * @author aacitelli
 */
@Repository
public interface PaymentOptionRepository
    extends JpaRepository<Installment, Long>, JpaSpecificationExecutor<Installment> {
  // Derived Query - using method naming convention
  // Optional<PaymentOption> findByOrganizationFiscalCodeAndNav(String organizationFiscalCode,
  // String nav);  // search only by nav
  Optional<Installment> findByOrganizationFiscalCodeAndIuv(
      String organizationFiscalCode, String iuv); // search only by iuv

  // TODO #naviuv: temporary regression management: search by nav or iuv
  Optional<Installment> findByOrganizationFiscalCodeAndIuvOrOrganizationFiscalCodeAndNav(
      String organizationFiscalCodeIuv, String iuv, String organizationFiscalCodeNav, String nav);

  // Derived Query - using method naming convention - get all PaymentOption by payment_position_id
  // and in the specified statuses
  List<Installment> findByPaymentPositionInAndStatusIn(
      List<PaymentPosition> paymentPositionList, List<PaymentOptionStatus> statusList);

  // Configuration Query
  @Modifying
  @Query(
          "update Installment i set i.sendSync = true " +
                  "where i.organizationFiscalCode = :organization " +
                  "and i.nav = :noticeNumber")
  int updatePaymentOptionSendSync(
          @Param(value = "organization") String organizationFiscalCode,
          @Param(value = "noticeNumber") String noticeNumber);
}
