package it.gov.pagopa.reporting.service;


import it.gov.pagopa.reporting.models.PaymentOption;
import it.gov.pagopa.reporting.models.RetryStep;
import org.xml.sax.Attributes;
import org.xml.sax.helpers.DefaultHandler;

import javax.persistence.Transient;
import java.util.ArrayList;
import java.util.List;

public class FlowXmlParser extends DefaultHandler {
    private final StringBuilder currentValue = new StringBuilder();
    private final List<PaymentOption> options = new ArrayList<>();
    @Transient
    private String iuv;
    @Transient
    private Integer transfer;

    public List<PaymentOption> getOptions() {
        return options;
    }

    @Override
    public void startElement(
            String uri,
            String localName,
            String qName,
            Attributes attributes) {

        // reset the tag value
        currentValue.setLength(0);
    }

    @Override
    public void endElement(String uri,
                           String localName,
                           String qName) {

        if (qName.equalsIgnoreCase("identificativoUnivocoVersamento")) {
            iuv = currentValue.toString();
        } else if (qName.equalsIgnoreCase("indiceDatiSingoloPagamento")) {
            transfer = Integer.parseInt(currentValue.toString());
            options.add(new PaymentOption(iuv, transfer, RetryStep.NONE.name()));
        }

    }

    @Override
    public void characters(char[] ch, int start, int length) {
        currentValue.append(ch, start, length);
    }

}
