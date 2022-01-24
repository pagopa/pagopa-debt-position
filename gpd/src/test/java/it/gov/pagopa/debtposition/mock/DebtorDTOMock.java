package it.gov.pagopa.debtposition.mock;

import java.time.LocalDateTime;
import java.time.temporal.ChronoUnit;

import it.gov.pagopa.debtposition.dto.DebtorDTO;
import it.gov.pagopa.debtposition.dto.PaymentOptionDTO;
import it.gov.pagopa.debtposition.dto.PaymentPositionDTO;
import it.gov.pagopa.debtposition.dto.TransferDTO;
import it.gov.pagopa.debtposition.model.enumeration.DebtPositionStatus;
import it.gov.pagopa.debtposition.model.enumeration.PaymentOptionStatus;
import it.gov.pagopa.debtposition.model.enumeration.Type;



public class DebtorDTOMock {
  public final static DebtorDTO getMock() {
    DebtorDTO debtorMock = new DebtorDTO();
    debtorMock.setFiscalCode("MRDPLL54H17D542L");
    debtorMock.setType(Type.F);
    debtorMock.setFullName("Mario Rossi");
    debtorMock.setPhone("3330987654");
    debtorMock.setStreetName("Via di novoli");
    debtorMock.setCivicNumber("50/2");
    debtorMock.setProvince("FI");
    debtorMock.setCountry("IT");
    debtorMock.setEmail("mario@firenze.it");
    debtorMock.setPostalCode("50100");

    debtorMock.addPaymentPosition(createPaymentPositionMock1());

    return debtorMock;
  }
  

  public static PaymentPositionDTO createPaymentPositionMock1() {

    PaymentPositionDTO pPMock = new PaymentPositionDTO();
    pPMock.setCompanyName("Comune di Firenze");
    pPMock.setOfficeName("Ufficio tributario");
    pPMock.setStatus(DebtPositionStatus.DRAFT);
    pPMock.addPaymentOptions(createPaymentOptionsMock1());
    
    return pPMock;
  }

  

  public static PaymentOptionDTO createPaymentOptionsMock1() {

    PaymentOptionDTO pOMock = new PaymentOptionDTO();
    pOMock.setAmount(1000);
    pOMock.setIuv("123456IUV");
    pOMock.setDueDate(LocalDateTime.now().plus(1, ChronoUnit.HOURS));
    pOMock.setIsPartialPayment(Boolean.FALSE);
    pOMock.setStatus(PaymentOptionStatus.PO_UNPAID);
    pOMock.addTransfers(createTransfersMock1()); 

    return pOMock;
  }

  
  public static TransferDTO createTransfersMock1() {
    TransferDTO tMock = new TransferDTO();
    tMock.setIdTransfer("id tranfer mock");
    tMock.setIban("IT75I0306902887100000300015");
    tMock.setAmount(1000);
    tMock.setRemittanceInformation("causale tari tefa");
    tMock.setCategory("10/22252/20");
    tMock.setPostalIban("IT82E0760113600000000118547");

    return tMock;
  }

}
