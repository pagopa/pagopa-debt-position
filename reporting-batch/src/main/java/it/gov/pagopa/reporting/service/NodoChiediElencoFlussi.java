package it.gov.pagopa.reporting.service;

import java.net.URL;
import javax.xml.ws.Holder;

import it.gov.pagopa.reporting.servicewsdl.FaultBean;
import it.gov.pagopa.reporting.servicewsdl.PagamentiTelematiciRPTservice;
import it.gov.pagopa.reporting.servicewsdl.PagamentiTelematiciRPT;
import it.gov.pagopa.reporting.servicewsdl.TipoElencoFlussiRendicontazione;

public class NodoChiediElencoFlussi {

    private static final URL WSD_URL = PagamentiTelematiciRPTservice.WSDL_LOCATION;
    private PagamentiTelematiciRPTservice ss;
    private PagamentiTelematiciRPT port;
    private Holder<FaultBean> nodoChiediElencoFlussiRendicontazioneFault;
    private Holder<TipoElencoFlussiRendicontazione> nodoChiediElencoFlussiRendicontazioneElencoFlussiRendicontazione;

    public void setNodoChiediElencoFlussiRendicontazioneFault(
            Holder<FaultBean> nodoChiediElencoFlussiRendicontazioneFault) {
        this.nodoChiediElencoFlussiRendicontazioneFault = nodoChiediElencoFlussiRendicontazioneFault;
    }

    public void setNodoChiediElencoFlussiRendicontazioneElencoFlussiRendicontazione(
            Holder<TipoElencoFlussiRendicontazione> nodoChiediElencoFlussiRendicontazioneElencoFlussiRendicontazione) {
        this.nodoChiediElencoFlussiRendicontazioneElencoFlussiRendicontazione = nodoChiediElencoFlussiRendicontazioneElencoFlussiRendicontazione;
    }

    private String nodoChiediElencoFlussiRendicontazioneIdentificativoIntermediarioPA = System
            .getenv("PAA_ID_INTERMEDIARIO");
    private String nodoChiediElencoFlussiRendicontazioneIdentificativoStazioneIntermediarioPA = System
            .getenv("PAA_STAZIONE_INT");
    private String nodoChiediElencoFlussiRendicontazionePassword = System.getenv("PAA_PASSWORD");

    public NodoChiediElencoFlussi() {

        ss = new PagamentiTelematiciRPTservice(WSD_URL);
        port = ss.getPagamentiTelematiciRPTPort();
    }

    public FaultBean getNodoChiediElencoFlussiRendicontazioneFault() {
        return nodoChiediElencoFlussiRendicontazioneFault != null ? nodoChiediElencoFlussiRendicontazioneFault.value
                : null;
    }

    public TipoElencoFlussiRendicontazione getNodoChiediElencoFlussiRendicontazioneElencoFlussiRendicontazione() {
        return nodoChiediElencoFlussiRendicontazioneElencoFlussiRendicontazione != null
                ? nodoChiediElencoFlussiRendicontazioneElencoFlussiRendicontazione.value
                : null;
    }

    public void nodoChiediElencoFlussiRendicontazione(String idPa) {

        var nodoChiediElencoFlussiRendicontazioneFaultLocal = new Holder<FaultBean>();
        var nodoChiediElencoFlussiRendicontazioneElencoFlussiRendicontazioneLocal = new Holder<TipoElencoFlussiRendicontazione>();

        port.nodoChiediElencoFlussiRendicontazione(nodoChiediElencoFlussiRendicontazioneIdentificativoIntermediarioPA,
                nodoChiediElencoFlussiRendicontazioneIdentificativoStazioneIntermediarioPA,
                nodoChiediElencoFlussiRendicontazionePassword, idPa, null,
                nodoChiediElencoFlussiRendicontazioneFaultLocal,
                nodoChiediElencoFlussiRendicontazioneElencoFlussiRendicontazioneLocal);

        setNodoChiediElencoFlussiRendicontazioneFault(nodoChiediElencoFlussiRendicontazioneFaultLocal);
        setNodoChiediElencoFlussiRendicontazioneElencoFlussiRendicontazione(
                nodoChiediElencoFlussiRendicontazioneElencoFlussiRendicontazioneLocal);
    }

}
