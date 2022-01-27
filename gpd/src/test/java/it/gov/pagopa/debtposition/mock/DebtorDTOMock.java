package it.gov.pagopa.debtposition.mock;

import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.time.temporal.ChronoUnit;

import it.gov.pagopa.debtposition.dto.DebtorDTO;
import it.gov.pagopa.debtposition.dto.PaymentOptionDTO;
import it.gov.pagopa.debtposition.dto.PaymentPositionDTO;
import it.gov.pagopa.debtposition.dto.TransferDTO;
import it.gov.pagopa.debtposition.model.enumeration.DebtPositionStatus;
import it.gov.pagopa.debtposition.model.enumeration.PaymentOptionStatus;
import it.gov.pagopa.debtposition.model.enumeration.Type;



public class DebtorDTOMock {
    public final static DebtorDTO getMock1() {
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
    
    public final static DebtorDTO getMock2() {
        DebtorDTO debtorMock = new DebtorDTO();
        debtorMock.setFiscalCode("FRNPLL54H17D542L");
        debtorMock.setType(Type.F);
        debtorMock.setFullName("Francesco Rizzo");
        debtorMock.setPhone("3330987654");
        debtorMock.setStreetName("Via di novoli");
        debtorMock.setCivicNumber("50/2");
        debtorMock.setProvince("NA");
        debtorMock.setCountry("IT");
        debtorMock.setEmail("francesco@napoli.it");
        debtorMock.setPostalCode("80100");

        debtorMock.addPaymentPosition(createPaymentPositionMock1());

        return debtorMock;
    }
    
    public final static DebtorDTO getMock3() {
        DebtorDTO debtorMock = new DebtorDTO();
        debtorMock.setFiscalCode("GNAPLL54H17D542L");
        debtorMock.setType(Type.F);
        debtorMock.setFullName("Gina Pane");
        debtorMock.setPhone("3330987654");
        debtorMock.setStreetName("Via di novoli");
        debtorMock.setCivicNumber("50/2");
        debtorMock.setProvince("AQ");
        debtorMock.setCountry("IT");
        debtorMock.setEmail("gina@aquila.it");
        debtorMock.setPostalCode("67100");

        debtorMock.addPaymentPosition(createPaymentPositionMock1());

        return debtorMock;
    }
    
    public final static DebtorDTO getMock4() {
        DebtorDTO debtorMock = new DebtorDTO();
        debtorMock.setFiscalCode("JHNPLL54H17D542L");
        debtorMock.setType(Type.F);
        debtorMock.setFullName("John Cina");
        debtorMock.setPhone("3330987654");
        debtorMock.setStreetName("Via di novoli");
        debtorMock.setCivicNumber("50/2");
        debtorMock.setProvince("TO");
        debtorMock.setCountry("IT");
        debtorMock.setEmail("John@torino.it");
        debtorMock.setPostalCode("10121");

        debtorMock.addPaymentPosition(createPaymentPositionMock1());

        return debtorMock;
    }
    
    public final static DebtorDTO get500Mock1() {
        DebtorDTO debtorMock = new DebtorDTO();
        // manca il codice fiscale => deve dare errore 500
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

    public final static DebtorDTO getMultiplePPMock1() {
        DebtorDTO debtorMock = new DebtorDTO();
        debtorMock.setFiscalCode("CPRPLL54H17D542L");
        debtorMock.setType(Type.F);
        debtorMock.setFullName("Marco Bianchi");
        debtorMock.setPhone("3330987654");
        debtorMock.setStreetName("Via di novoli");
        debtorMock.setCivicNumber("50/2");
        debtorMock.setProvince("RM");
        debtorMock.setCountry("IT");
        debtorMock.setEmail("marco@roma.it");
        debtorMock.setPostalCode("00100");

        debtorMock.addPaymentPosition(createPaymentPositionMultipleMock1());
        debtorMock.addPaymentPosition(createPaymentPositionMultipleMock2());


        return debtorMock;
    }
    
    public final static DebtorDTO getMultiplePPMock2() {
        DebtorDTO debtorMock = new DebtorDTO();
        debtorMock.setFiscalCode("PIVA12345678");
        debtorMock.setType(Type.G);
        debtorMock.setFullName("Cipriani Srl");
        debtorMock.setPhone("3330987654");
        debtorMock.setStreetName("Via di novoli");
        debtorMock.setCivicNumber("50/2");
        debtorMock.setProvince("RM");
        debtorMock.setCountry("IT");
        debtorMock.setEmail("cipriani@roma.it");
        debtorMock.setPostalCode("00100");

        debtorMock.addPaymentPosition(createPaymentPositionMultipleMock1());
        debtorMock.addPaymentPosition(createPaymentPositionMultipleMock2());


        return debtorMock;
    }
    
    public final static DebtorDTO getMultiplePPMock3() {
        DebtorDTO debtorMock = new DebtorDTO();
        debtorMock.setFiscalCode("PIVA0000000");
        debtorMock.setType(Type.G);
        debtorMock.setFullName("DGS Spa");
        debtorMock.setPhone("3330987654");
        debtorMock.setStreetName("Via di novoli");
        debtorMock.setCivicNumber("50/2");
        debtorMock.setProvince("RM");
        debtorMock.setCountry("IT");
        debtorMock.setEmail("dgs@roma.it");
        debtorMock.setPostalCode("00100");

        debtorMock.addPaymentPosition(createPaymentPositionMultipleMock1());
        debtorMock.addPaymentPosition(createPaymentPositionMultipleMock2());


        return debtorMock;
    }
    
    public final static DebtorDTO getMultiplePPMock4() {
        DebtorDTO debtorMock = new DebtorDTO();
        debtorMock.setFiscalCode("BVNANT4H17D542L");
        debtorMock.setType(Type.F);
        debtorMock.setFullName("Antonino Benevento");
        debtorMock.setPhone("3330987654");
        debtorMock.setStreetName("Via di novoli");
        debtorMock.setCivicNumber("50/2");
        debtorMock.setProvince("RM");
        debtorMock.setCountry("IT");
        debtorMock.setEmail("antonino@roma.it");
        debtorMock.setPostalCode("00100");

        debtorMock.addPaymentPosition(createPaymentPositionMultipleMock1());
        debtorMock.addPaymentPosition(createPaymentPositionMultipleMock2());


        return debtorMock;
    }
    
    public final static DebtorDTO getMultiplePPMock5() {
        DebtorDTO debtorMock = new DebtorDTO();
        debtorMock.setFiscalCode("PIVA1111111");
        debtorMock.setType(Type.G);
        debtorMock.setFullName("Mediacon Spa");
        debtorMock.setPhone("3330987654");
        debtorMock.setStreetName("Via di novoli");
        debtorMock.setCivicNumber("50/2");
        debtorMock.setProvince("RM");
        debtorMock.setCountry("IT");
        debtorMock.setEmail("mediacon@roma.it");
        debtorMock.setPostalCode("00100");

        debtorMock.addPaymentPosition(createPaymentPositionMultipleMock1());
        debtorMock.addPaymentPosition(createPaymentPositionMultipleMock2());


        return debtorMock;
    }
    
    public final static DebtorDTO get400Mock() {
        DebtorDTO debtorMock = new DebtorDTO();
        debtorMock.setFiscalCode("VRDPLL54H17D542L");
        debtorMock.setType(Type.F);
        debtorMock.setFullName("Antonio Verdi");
        debtorMock.setPhone("3330987654");
        debtorMock.setStreetName("Via di novoli");
        debtorMock.setCivicNumber("50/2");
        debtorMock.setProvince("RM");
        debtorMock.setCountry("IT");
        debtorMock.setEmail("antonio@roma.it");
        debtorMock.setPostalCode("00100");

        debtorMock.addPaymentPosition(createPaymentPosition400Mock1());


        return debtorMock;
    }


    public static PaymentPositionDTO createPaymentPositionMock1() {

        PaymentPositionDTO pPMock = new PaymentPositionDTO();
        pPMock.setIupd("12345678901IUPDMOCK1");
        pPMock.setCompanyName("Comune di Firenze");
        pPMock.setOfficeName("Ufficio tributario");
        pPMock.setStatus(DebtPositionStatus.DRAFT);
        pPMock.addPaymentOptions(createPaymentOptionsMock1());

        return pPMock;
    }
    
    public static PaymentPositionDTO createPaymentPositionMultipleMock1() {

        PaymentPositionDTO pPMock = new PaymentPositionDTO();
        pPMock.setIupd("12345678901IUPDMULTIPLEMOCK1");
        pPMock.setCompanyName("Comune di Roma");
        pPMock.setOfficeName("Ufficio tributario");
        pPMock.setStatus(DebtPositionStatus.DRAFT);
        pPMock.addPaymentOptions(createPaymentOptionsMultipleMock1());
        pPMock.addPaymentOptions(createPaymentOptionsMultipleMock2());

        return pPMock;
    }
    
    public static PaymentPositionDTO createPaymentPositionMultipleMock2() {

        PaymentPositionDTO pPMock = new PaymentPositionDTO();
        pPMock.setIupd("12345678901IUPDMULTIPLEMOCK2");
        pPMock.setCompanyName("Comune di Roma");
        pPMock.setOfficeName("Ufficio condono");
        pPMock.setStatus(DebtPositionStatus.DRAFT);
        pPMock.addPaymentOptions(createPaymentOptionsMultipleMock3());
        pPMock.addPaymentOptions(createPaymentOptionsMultipleMock4());

        return pPMock;
    }
    
    public static PaymentPositionDTO createPaymentPosition400Mock1() {

        PaymentPositionDTO pPMock = new PaymentPositionDTO();
        pPMock.setIupd("12345678901IUPD400MOCK1");
        pPMock.setCompanyName("Comune di Roma");
        pPMock.setOfficeName("Ufficio tributario");
        pPMock.setStatus(DebtPositionStatus.DRAFT);
        pPMock.addPaymentOptions(createPaymentOptions400Mock1());

        return pPMock;
    }



    public static PaymentOptionDTO createPaymentOptionsMock1() {

        PaymentOptionDTO pOMock = new PaymentOptionDTO();
        pOMock.setAmount(1000);
        pOMock.setIuv("123456IUVMOCK1");
        pOMock.setDueDate(LocalDateTime.now(ZoneOffset.UTC).plus(2, ChronoUnit.HOURS));
        pOMock.setIsPartialPayment(Boolean.FALSE);
        pOMock.setStatus(PaymentOptionStatus.PO_UNPAID);
        pOMock.addTransfers(createTransfersMock1()); 

        return pOMock;
    }
    
    public static PaymentOptionDTO createPaymentOptionsMultipleMock1() {

        PaymentOptionDTO pOMock = new PaymentOptionDTO();
        pOMock.setAmount(1000);
        pOMock.setIuv("123456IUVMULTIPLEMOCK1");
        pOMock.setDueDate(LocalDateTime.now(ZoneOffset.UTC).plus(2, ChronoUnit.HOURS));
        pOMock.setIsPartialPayment(Boolean.FALSE);
        pOMock.setStatus(PaymentOptionStatus.PO_UNPAID);
        pOMock.addTransfers(createTransfersMultipleMock1()); 

        return pOMock;
    }
    
    public static PaymentOptionDTO createPaymentOptionsMultipleMock2() {

        PaymentOptionDTO pOMock = new PaymentOptionDTO();
        pOMock.setAmount(500);
        pOMock.setIuv("123456IUVMULTIPLEMOCK2");
        pOMock.setDueDate(LocalDateTime.now(ZoneOffset.UTC).plus(7, ChronoUnit.DAYS));
        pOMock.setIsPartialPayment(Boolean.FALSE);
        pOMock.setStatus(PaymentOptionStatus.PO_UNPAID);
        pOMock.addTransfers(createTransfersMultipleMock2()); 

        return pOMock;
    }
    
    public static PaymentOptionDTO createPaymentOptionsMultipleMock3() {

        PaymentOptionDTO pOMock = new PaymentOptionDTO();
        pOMock.setAmount(10000);
        pOMock.setIuv("123456IUVMULTIPLEMOCK3");
        pOMock.setDueDate(LocalDateTime.now(ZoneOffset.UTC).plus(2, ChronoUnit.HOURS));
        pOMock.setIsPartialPayment(Boolean.FALSE);
        pOMock.setStatus(PaymentOptionStatus.PO_UNPAID);
        pOMock.addTransfers(createTransfersMultipleMock3()); 

        return pOMock;
    }
    
    public static PaymentOptionDTO createPaymentOptionsMultipleMock4() {

        PaymentOptionDTO pOMock = new PaymentOptionDTO();
        pOMock.setAmount(5000);
        pOMock.setIuv("123456IUVMULTIPLEMOCK4");
        pOMock.setDueDate(LocalDateTime.now(ZoneOffset.UTC).plus(2, ChronoUnit.HOURS));
        pOMock.setIsPartialPayment(Boolean.TRUE);
        pOMock.setStatus(PaymentOptionStatus.PO_UNPAID);
        pOMock.addTransfers(createTransfersMultipleMock4()); 
        pOMock.addTransfers(createTransfersMultipleMock5()); 

        return pOMock;
    }
    
    public static PaymentOptionDTO createPaymentOptions400Mock1() {

        PaymentOptionDTO pOMock = new PaymentOptionDTO();
        pOMock.setAmount(1000);
        pOMock.setIuv("123456IUV400MOCK1");
        // due_date < current date => deve dare errore 400
        pOMock.setDueDate(LocalDateTime.now(ZoneOffset.UTC).minus(1, ChronoUnit.DAYS));
        pOMock.setIsPartialPayment(Boolean.FALSE);
        pOMock.setStatus(PaymentOptionStatus.PO_UNPAID);
        pOMock.addTransfers(createTransfersMock1()); 

        return pOMock;
    }


    public static TransferDTO createTransfersMock1() {
        TransferDTO tMock = new TransferDTO();
        tMock.setIdTransfer("id tranfer mock 1");
        tMock.setIban("IT75I0306902887100000300015");
        tMock.setAmount(1000);
        tMock.setRemittanceInformation("causale mock 1");
        tMock.setCategory("10/22252/20");
        tMock.setPostalIban("IT82E0760113600000000118547");

        return tMock;
    }
    
    
    public static TransferDTO createTransfersMultipleMock1() {
        TransferDTO tMock = new TransferDTO();
        tMock.setIdTransfer("id tranfer mock multiple 1");
        tMock.setIban("IT75I0306902887100000300005");
        tMock.setAmount(1000);
        tMock.setRemittanceInformation("causale mock multiple 1");
        tMock.setCategory("10/22252/20");
        tMock.setPostalIban("IT82E0760113600000000118547");

        return tMock;
    }
    
    public static TransferDTO createTransfersMultipleMock2() {
        TransferDTO tMock = new TransferDTO();
        tMock.setIdTransfer("id tranfer mock multiple 2");
        tMock.setIban("IT75I0306902887100000300006");
        tMock.setAmount(500);
        tMock.setRemittanceInformation("causale mock multiple 2");
        tMock.setCategory("10/22252/20");
        tMock.setPostalIban("IT82E0760113600000000118547");

        return tMock;
    }
    
    public static TransferDTO createTransfersMultipleMock3() {
        TransferDTO tMock = new TransferDTO();
        tMock.setIdTransfer("id tranfer mock multiple 3");
        tMock.setIban("IT75I0306902887100000300007");
        tMock.setAmount(10000);
        tMock.setRemittanceInformation("causale mock multiple 3");
        tMock.setCategory("10/22252/20");
        tMock.setPostalIban("IT82E0760113600000000118547");

        return tMock;
    }
    
    public static TransferDTO createTransfersMultipleMock4() {
        TransferDTO tMock = new TransferDTO();
        tMock.setIdTransfer("id tranfer mock multiple 4");
        tMock.setIban("IT75I0306902887100000300007");
        tMock.setAmount(2500);
        tMock.setRemittanceInformation("causale mock multiple 4");
        tMock.setCategory("10/22252/20");
        tMock.setPostalIban("IT82E0760113600000000118547");

        return tMock;
    }
    
    public static TransferDTO createTransfersMultipleMock5() {
        TransferDTO tMock = new TransferDTO();
        tMock.setIdTransfer("id tranfer mock multiple 5");
        tMock.setIban("IT75I0306902887100000300007");
        tMock.setAmount(2500);
        tMock.setRemittanceInformation("causale mock multiple 5");
        tMock.setCategory("10/22252/20");
        tMock.setPostalIban("IT82E0760113600000000118547");

        return tMock;
    }

}
