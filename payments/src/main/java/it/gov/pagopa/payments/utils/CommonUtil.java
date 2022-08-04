package it.gov.pagopa.payments.utils;

import it.gov.pagopa.payments.entity.ReceiptEntity;
import it.gov.pagopa.payments.model.PageInfo;
import it.gov.pagopa.payments.model.PaymentsResult;
import lombok.experimental.UtilityClass;
import org.xml.sax.SAXException;

import javax.xml.XMLConstants;
import javax.xml.stream.XMLInputFactory;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamReader;
import javax.xml.transform.stax.StAXSource;
import javax.xml.validation.SchemaFactory;
import javax.xml.validation.Validator;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.IOException;


@UtilityClass
public class CommonUtil {


    /**
     * @param receipts Page returned from the database
     * @return return the page info
     */

    public PageInfo buildPageInfo(PaymentsResult<ReceiptEntity> receipts) {
        return PageInfo.builder()
                .limit(receipts.getPageSize())
                .morePages(receipts.isHasMoreResults())
                .itemsFound(receipts.getLength())
                .page(receipts.getCurrentPageNumber())
                .build();
    }


    /**
     * @param xml    file XML to validate
     * @param xsdUrl url of XSD
     * @throws SAXException       if XML is not valid
     * @throws IOException        if XSD schema not found
     * @throws XMLStreamException error during read XML
     */
    public static void syntacticValidationXml(byte[] xml, File xsdUrl) throws SAXException, IOException, XMLStreamException {
        SchemaFactory factory = SchemaFactory.newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI);
        // to be compliant, prohibit the use of all protocols by external entities:
        factory.setProperty(XMLConstants.ACCESS_EXTERNAL_DTD, "");
        factory.setProperty(XMLConstants.ACCESS_EXTERNAL_SCHEMA, "");

        javax.xml.validation.Schema schema = factory.newSchema(xsdUrl);
        Validator validator = schema.newValidator();

        XMLInputFactory xmlInputFactory = XMLInputFactory.newInstance();
        // to be compliant, completely disable DOCTYPE declaration:
        xmlInputFactory.setProperty(XMLInputFactory.SUPPORT_DTD, false);

        XMLStreamReader xmlStreamReader = xmlInputFactory.createXMLStreamReader(new ByteArrayInputStream(xml));
        StAXSource source = new StAXSource(xmlStreamReader);
        validator.validate(source);
    }


}
