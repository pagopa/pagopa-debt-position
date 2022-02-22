package it.gov.pagopa.reporting.service;

import java.io.IOException;
import java.net.URL;
import java.security.KeyManagementException;
import java.security.KeyStoreException;
import java.security.NoSuchAlgorithmException;
import java.security.UnrecoverableKeyException;
import java.security.cert.CertificateException;
import java.security.spec.InvalidKeySpecException;

import javax.activation.DataHandler;
import javax.xml.ws.BindingProvider;
import javax.xml.ws.Holder;

import it.gov.pagopa.reporting.config.SslConfig;
import it.gov.pagopa.reporting.servicewsdl.FaultBean;
import it.gov.pagopa.reporting.servicewsdl.PagamentiTelematiciRPTservice;
import it.gov.pagopa.reporting.servicewsdl.PagamentiTelematiciRPT;

public class NodeService {

    private static final URL WSD_URL = PagamentiTelematiciRPTservice.WSDL_LOCATION;

    private String identificativoIntemediarioPA;
    private String identificativoStazioneIntermediarioPA;
    private String paaPassword;
    private PagamentiTelematiciRPTservice ss;
    private PagamentiTelematiciRPT port;
    private Holder<FaultBean> nodoChiediFlussoRendicontazioneFault;
    private Holder<DataHandler> reportingXml;

    public NodeService(String identificativoIntemediarioPA, String identificativoStazioneIntermediarioPA,
            String paaPassword) {

        this.identificativoIntemediarioPA = identificativoIntemediarioPA;
        this.identificativoStazioneIntermediarioPA = identificativoStazioneIntermediarioPA;
        this.paaPassword = paaPassword;
        this.ss = new PagamentiTelematiciRPTservice(WSD_URL);
        this.port = ss.getPagamentiTelematiciRPTPort();
    }

    public void setNodoChiediFlussoRendicontazioneFault(Holder<FaultBean> fault) {
        this.nodoChiediFlussoRendicontazioneFault = fault;
    }

    public FaultBean getNodoChiediFlussoRendicontazioneFault() {

        return nodoChiediFlussoRendicontazioneFault != null ? nodoChiediFlussoRendicontazioneFault.value : null;
    }

    public void setNodoChiediFlussoRendicontazioneXmlReporting(Holder<DataHandler> reportingXml) {

        this.reportingXml = reportingXml;
    }

    public DataHandler getNodoChiediElencoFlussiRendicontazioneXmlReporting() {

        return this.reportingXml != null ? this.reportingXml.value : null;
    }

    public void callNodoChiediFlussoRendicontazione(String idPa, String idFlow) {

        Holder<FaultBean> fault = new Holder<>();
        Holder<DataHandler> result = new Holder<>();

        port.nodoChiediFlussoRendicontazione(this.identificativoIntemediarioPA,
                this.identificativoStazioneIntermediarioPA, this.paaPassword, idPa, null, idFlow, fault, result);

        setNodoChiediFlussoRendicontazioneFault(fault);
        setNodoChiediFlussoRendicontazioneXmlReporting(result);
    }

}
