package it.gov.pagopa.debtposition.repository;

import it.gov.pagopa.debtposition.entity.Installment;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.util.Optional;

public interface InstallmentRepository extends JpaRepository<Installment, Long>, JpaSpecificationExecutor<Installment> {
    // Derived Query - using method naming convention
    // Optional<Installment> findByOrganizationFiscalCodeAndNav(String organizationFiscalCode,
    // search only by iuv
    Optional<Installment> findByOrganizationFiscalCodeAndIuv(
            String organizationFiscalCode, String iuv);

    // TODO #naviuv: temporary regression management: search by nav or iuv
    Optional<Installment> findByOrganizationFiscalCodeAndIuvOrOrganizationFiscalCodeAndNav(
            String organizationFiscalCodeIuv, String iuv, String organizationFiscalCodeNav, String nav);


    // Configuration Query
    @Modifying
    @Query(
            "update Installment inst set inst.sendSync = true " +
                    "where inst.organizationFiscalCode = :organization " +
                    "and inst.nav = :noticeNumber")
    int updateInstallmentSendSync(
            @Param(value = "organization") String organizationFiscalCode,
            @Param(value = "noticeNumber") String noticeNumber);
}
