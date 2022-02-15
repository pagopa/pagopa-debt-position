package it.gov.pagopa.reporting.service;


import org.xml.sax.Attributes;
import org.xml.sax.helpers.DefaultHandler;

import java.util.ArrayList;
import java.util.List;

public class FlowXmlParser extends DefaultHandler {
    private List<String> options = new ArrayList<>();

    public List<String> getOptions() {
        return options;
    }

    private final StringBuilder currentValue = new StringBuilder();

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
            options.add(currentValue.toString());
        }

    }

    @Override
    public void characters(char[] ch, int start, int length) {

        currentValue.append(ch, start, length);

    }

}