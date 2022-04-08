package it.gov.pagopa.reporting.service;

import com.sun.xml.ws.client.ClientTransportException;
import it.gov.pagopa.reporting.servicewsdl.FaultBean;
import it.gov.pagopa.reporting.servicewsdl.PagamentiTelematiciRPT;
import it.gov.pagopa.reporting.servicewsdl.PagamentiTelematiciRPTservice;

import javax.activation.DataHandler;
import javax.xml.ws.Holder;
import java.net.URL;

public class NodeService {

    private static final URL WSD_URL = PagamentiTelematiciRPTservice.WSDL_LOCATION;

    private final String identificativoIntemediarioPA;
    private final String identificativoStazioneIntermediarioPA;
    private final String paaPassword;
    private final PagamentiTelematiciRPTservice ss;
    private final PagamentiTelematiciRPT port;
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

    public FaultBean getNodoChiediFlussoRendicontazioneFault() {

        return nodoChiediFlussoRendicontazioneFault != null ? nodoChiediFlussoRendicontazioneFault.value : null;
    }

    public void setNodoChiediFlussoRendicontazioneFault(Holder<FaultBean> fault) {
        this.nodoChiediFlussoRendicontazioneFault = fault;
    }

    public void setNodoChiediFlussoRendicontazioneXmlReporting(Holder<DataHandler> reportingXml) {

        this.reportingXml = reportingXml;
    }

    public DataHandler getNodoChiediElencoFlussiRendicontazioneXmlReporting() {

        return this.reportingXml != null ? this.reportingXml.value : null;
    }

    public void callNodoChiediFlussoRendicontazione(String idPa, String idFlow) throws ClientTransportException {

        Holder<FaultBean> fault = new Holder<>();
        Holder<DataHandler> result = new Holder<>();

        port.nodoChiediFlussoRendicontazione(this.identificativoIntemediarioPA,
                this.identificativoStazioneIntermediarioPA, this.paaPassword, idPa, null, idFlow, fault, result);

        setNodoChiediFlussoRendicontazioneFault(fault);
        setNodoChiediFlussoRendicontazioneXmlReporting(result);
    }

}
