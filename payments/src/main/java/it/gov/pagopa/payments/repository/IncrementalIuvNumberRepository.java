package it.gov.pagopa.payments.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import it.gov.pagopa.payments.entity.IncrementalIuvNumber;

@Repository
public interface IncrementalIuvNumberRepository extends JpaRepository<IncrementalIuvNumber, Long> {
    
    IncrementalIuvNumber findByIdDominioPaAndAnno(String idDomminioPa,Integer anno);
    
}
