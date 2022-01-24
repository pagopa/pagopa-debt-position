package it.gov.pagopa.debtposition.mock;

import java.time.LocalDateTime;
import java.time.temporal.ChronoUnit;

import it.gov.pagopa.debtposition.entity.Debtor;
import it.gov.pagopa.debtposition.entity.PaymentOption;
import it.gov.pagopa.debtposition.entity.PaymentPosition;
import it.gov.pagopa.debtposition.entity.Transfer;
import it.gov.pagopa.debtposition.model.enumeration.DebtPositionStatus;
import it.gov.pagopa.debtposition.model.enumeration.PaymentOptionStatus;
import it.gov.pagopa.debtposition.model.enumeration.Type;



public class DebtorMock {
  public final static Debtor getMock() {
    Debtor debtorMock = new Debtor();
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
  

  public static PaymentPosition createPaymentPositionMock1() {

    PaymentPosition pPMock = new PaymentPosition();
    pPMock.setCompanyName("Comune di Firenze");
    pPMock.setOfficeName("Ufficio tributario");
    pPMock.setStatus(DebtPositionStatus.DRAFT);
    pPMock.addPaymentOptions(createPaymentOptionsMock1());
    //pPMock.setDebtor(DebtorMock.getMock());
    
    return pPMock;
  }

  

  public static PaymentOption createPaymentOptionsMock1() {

    PaymentOption pOMock = new PaymentOption();
    pOMock.setAmount(1000);
    pOMock.setDueDate(LocalDateTime.now().plus(1, ChronoUnit.HOURS));
    pOMock.setIsPartialPayment(Boolean.FALSE);
    pOMock.setStatus(PaymentOptionStatus.PO_UNPAID);
    pOMock.addTransfers(createTransfersMock1());
    //pOMock.setPaymentPosition(createPaymentPositionMock1()); 

    return pOMock;
  }

  
  public static Transfer createTransfersMock1() {
    Transfer tMock = new Transfer();
    tMock.setIdTransfer("id tranfer mock");
    tMock.setIban("IT75I0306902887100000300015");
    tMock.setAmount(1000);
    tMock.setRemittanceInformation("causale tari tefa");
    tMock.setCategory("10/22252/20");
    tMock.setPostalIban("IT82E0760113600000000118547");
    //tMock.setPaymentOption(createPaymentOptionsMock1());

    return tMock;
  }

}
