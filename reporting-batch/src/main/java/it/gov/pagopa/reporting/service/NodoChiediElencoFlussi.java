package it.gov.pagopa.hubpa.service;

import java.io.IOException;
import java.net.URL;
import java.security.KeyManagementException;
import java.security.KeyStoreException;
import java.security.NoSuchAlgorithmException;
import java.security.UnrecoverableKeyException;
import java.security.cert.CertificateException;
import java.security.spec.InvalidKeySpecException;

import javax.xml.ws.BindingProvider;
import javax.xml.ws.Holder;

import it.gov.pagopa.hubpa.servicewsdl.FaultBean;
import it.gov.pagopa.hubpa.servicewsdl.PagamentiTelematiciRPTservice;
import it.gov.pagopa.hubpa.servicewsdl.PagamentiTelematiciRPT;
import it.gov.pagopa.hubpa.servicewsdl.TipoElencoFlussiRendicontazione;
import it.gov.pagopa.hubpa.utils.SslContextUtil;

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
    private String cert = System.getenv("CERT");
    private String key = System.getenv("KEY_PKCS8");
    private String certPassword = System.getenv("CERT_PASSWORD");

    public NodoChiediElencoFlussi() {

        ss = new PagamentiTelematiciRPTservice(WSD_URL);
        port = ss.getPagamentiTelematiciRPTPort();
    }

    public void setSslContext() throws UnrecoverableKeyException, CertificateException, IOException,
            NoSuchAlgorithmException, KeyStoreException, InvalidKeySpecException, KeyManagementException {

        ((BindingProvider) port).getRequestContext().put("com.sun.xml.ws.transport.https.client.SSLSocketFactory",
                SslContextUtil.getSslContext(cert, key, certPassword));

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
