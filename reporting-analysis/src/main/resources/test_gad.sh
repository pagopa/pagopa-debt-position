#!/bin/bash

curl --key ke2.key --cert cert.cert \
--location --request POST 'https://gad.test.pagopa.gov.it/openspcoop2/proxy/PA/RPT6T' \
--header 'SOAPAction: nodoChiediElencoFlussiRendicontazione' \
--header 'Content-Type: text/xml' \
--data-raw '  <soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/" xmlns:ws="http://ws.pagamenti.telematici.gov/">
   <soapenv:Header/>
   <soapenv:Body>
      <ws:nodoChiediElencoFlussiRendicontazione>
         <identificativoIntermediarioPA>15376371009</identificativoIntermediarioPA>
         <identificativoStazioneIntermediarioPA>15376371009_03</identificativoStazioneIntermediarioPA>
         <password>PagoPA03Test</password>
      </ws:nodoChiediElencoFlussiRendicontazione>
   </soapenv:Body>
</soapenv:Envelope>'