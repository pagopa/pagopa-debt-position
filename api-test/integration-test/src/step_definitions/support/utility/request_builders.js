const { buildStringFromDate, makeidNumber, addDays } = require("./helpers");

function buildActivatePaymentNoticeRequest(bundle) {
    return `<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/" xmlns:nod="http://pagopa-api.pagopa.gov.it/node/nodeForPsp.xsd">
        <soapenv:Header/>
        <soapenv:Body>
            <nod:activatePaymentNoticeReq>
                <idPSP>${bundle.psp.id}</idPSP>
                <idBrokerPSP>${bundle.psp.brokerId}</idBrokerPSP>
                <idChannel>${bundle.psp.channelId}</idChannel>
                <password>${bundle.psp.channelPassword}</password>
                <idempotencyKey>${bundle.debtPositionCalculation.idempotencyKey}</idempotencyKey>
                <qrCode>
                    <fiscalCode>${bundle.creditorInstitution.id}</fiscalCode>
                    <noticeNumber>${bundle.debtPositionCalculation.noticeNumber}</noticeNumber>
                </qrCode>
                <expirationTime>6000</expirationTime>
                <amount>${bundle.debtPositionCalculation.decimalAmount}</amount>
            </nod:activatePaymentNoticeReq>
        </soapenv:Body>
    </soapenv:Envelope>`;
}

function buildReportFlowCreationRequest(bundle, reportFlow) {
    return `<Envelope xmlns="http://schemas.xmlsoap.org/soap/envelope/">
                <Body>
                    <nodoInviaFlussoRendicontazione xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns="http://ws.pagamenti.telematici.gov/">
                        <identificativoPSP xmlns="">${bundle.psp.id}</identificativoPSP>
                        <identificativoIntermediarioPSP xmlns="">${bundle.psp.brokerId}</identificativoIntermediarioPSP>
                        <identificativoCanale xmlns="">${bundle.psp.channelId}</identificativoCanale>
                        <password xmlns="">${bundle.psp.channelPassword}</password>
                        <identificativoDominio xmlns="">${bundle.creditorInstitution.id}</identificativoDominio>
                        <identificativoFlusso xmlns="">${bundle.flow.id}</identificativoFlusso>
                        <dataOraFlusso xmlns="">${bundle.flow.dateAndTime}</dataOraFlusso>
                        <xmlRendicontazione xmlns="">${reportFlow}</xmlRendicontazione>
                    </nodoInviaFlussoRendicontazione>
                </Body>
            </Envelope>`;
}

function buildReportFlowForDebtPositionRequest(bundle) {
    let flowDate = buildStringFromDate(addDays(0));
    bundle.flow = {
        id: `${flowDate}${bundle.psp.id}-S${makeidNumber(9)}`,
        date: `${flowDate}`,
        dateAndTime: `${flowDate}T23:59:59`
    }
    return `<pay_i:FlussoRiversamento
                xmlns:pay_i="http://www.digitpa.gov.it/schemas/2011/Pagamenti/"
                xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="http://www.digitpa.gov.it/schemas/2011/Pagamenti/ FlussoRendicontazione_v_1_0_1.xsd ">
                <pay_i:versioneOggetto>1.0</pay_i:versioneOggetto>
                <pay_i:identificativoFlusso>${bundle.flow.id}</pay_i:identificativoFlusso>
                <pay_i:dataOraFlusso>${bundle.flow.dateAndTime}</pay_i:dataOraFlusso>
                <pay_i:identificativoUnivocoRegolamento>${bundle.debtPosition.paymentOption[0].iuv}-${flowDate.replace('-', '').replace('-', '')}235959999</pay_i:identificativoUnivocoRegolamento>
                <pay_i:dataRegolamento>${bundle.flow.date}</pay_i:dataRegolamento>
                <pay_i:istitutoMittente>
                    <pay_i:identificativoUnivocoMittente>
                        <pay_i:tipoIdentificativoUnivoco>G</pay_i:tipoIdentificativoUnivoco>
                        <pay_i:codiceIdentificativoUnivoco>${bundle.psp.id}</pay_i:codiceIdentificativoUnivoco>
                    </pay_i:identificativoUnivocoMittente>
                    <pay_i:denominazioneMittente>${bundle.psp.businessName}</pay_i:denominazioneMittente>
                </pay_i:istitutoMittente>
                <pay_i:codiceBicBancaDiRiversamento>${bundle.psp.bic}</pay_i:codiceBicBancaDiRiversamento>
                <pay_i:istitutoRicevente>
                    <pay_i:identificativoUnivocoRicevente>
                        <pay_i:tipoIdentificativoUnivoco>G</pay_i:tipoIdentificativoUnivoco>
                        <pay_i:codiceIdentificativoUnivoco>${bundle.creditorInstitution.id}</pay_i:codiceIdentificativoUnivoco>
                    </pay_i:identificativoUnivocoRicevente>
                    <pay_i:denominazioneRicevente>${bundle.creditorInstitution.businessName}</pay_i:denominazioneRicevente>
                </pay_i:istitutoRicevente>
                <pay_i:numeroTotalePagamenti>2</pay_i:numeroTotalePagamenti>
                <pay_i:importoTotalePagamenti>${bundle.debtPosition.paymentOption[0].amount / 100}.00</pay_i:importoTotalePagamenti>
                <pay_i:datiSingoliPagamenti>
                    <pay_i:identificativoUnivocoVersamento>${bundle.debtPosition.paymentOption[0].iuv}-${bundle.debtPosition.paymentOption[0].transfer[0].idTransfer}-${flowDate}</pay_i:identificativoUnivocoVersamento>
                    <pay_i:identificativoUnivocoRiscossione>IUR${makeidNumber(17)}</pay_i:identificativoUnivocoRiscossione>
                    <pay_i:indiceDatiSingoloPagamento>${bundle.debtPosition.paymentOption[0].transfer[0].idTransfer}</pay_i:indiceDatiSingoloPagamento>
                    <pay_i:singoloImportoPagato>${bundle.debtPosition.paymentOption[0].transfer[0].amount / 100}.00</pay_i:singoloImportoPagato>
                    <pay_i:codiceEsitoSingoloPagamento>0</pay_i:codiceEsitoSingoloPagamento>
                    <pay_i:dataEsitoSingoloPagamento>${bundle.debtPositionCalculation.applicationDate}</pay_i:dataEsitoSingoloPagamento>
                </pay_i:datiSingoliPagamenti>
                <pay_i:datiSingoliPagamenti>
                    <pay_i:identificativoUnivocoVersamento>${bundle.debtPosition.paymentOption[0].iuv}-${bundle.debtPosition.paymentOption[0].transfer[1].idTransfer}-${flowDate}</pay_i:identificativoUnivocoVersamento>
                    <pay_i:identificativoUnivocoRiscossione>IUR${makeidNumber(17)}</pay_i:identificativoUnivocoRiscossione>
                    <pay_i:indiceDatiSingoloPagamento>${bundle.debtPosition.paymentOption[0].transfer[1].idTransfer}</pay_i:indiceDatiSingoloPagamento>
                    <pay_i:singoloImportoPagato>${bundle.debtPosition.paymentOption[0].transfer[1].amount / 100}.00</pay_i:singoloImportoPagato>
                    <pay_i:codiceEsitoSingoloPagamento>0</pay_i:codiceEsitoSingoloPagamento>
                    <pay_i:dataEsitoSingoloPagamento>${bundle.debtPositionCalculation.applicationDate}</pay_i:dataEsitoSingoloPagamento>
                </pay_i:datiSingoliPagamenti>
            </pay_i:FlussoRiversamento>`;
}


function buildReportFlowsRetrieveRequest(bundle) {
    return `<Envelope xmlns="http://schemas.xmlsoap.org/soap/envelope/">
                <Body>
                    <nodoChiediElencoFlussiRendicontazione xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns="http://ws.pagamenti.telematici.gov/">
                        <identificativoIntermediarioPA xmlns="">${bundle.broker.id}</identificativoIntermediarioPA>
                        <identificativoStazioneIntermediarioPA xmlns="">${bundle.station.id}</identificativoStazioneIntermediarioPA>
                        <password xmlns="">${bundle.station.password}</password>
                        <identificativoDominio xmlns="">${bundle.creditorInstitution.id}</identificativoDominio>
                    </nodoChiediElencoFlussiRendicontazione>
                </Body>
            </Envelope>`;
}

function buildSendPaymentOutcomeRequest(bundle) {    
    return `<soapenv:Envelope>
        <soapenv:Body>
            <nod:sendPaymentOutcomeReq>
            <idPSP>${bundle.psp.id}</idPSP>
            <idBrokerPSP>${bundle.psp.brokerId}</idBrokerPSP>
            <idChannel>${bundle.psp.channelId}</idChannel>
            <password>${bundle.psp.channelPassword}</password>
            <idempotencyKey>${bundle.debtPositionCalculation.idempotencyKey}</idempotencyKey>
            <paymentTokens>
                <paymentToken>${bundle.debtPositionCalculation.paymentToken}</paymentToken>
            </paymentTokens>
            <outcome>OK</outcome>
            <details>
                <paymentMethod>creditCard</paymentMethod>
                <paymentChannel>app</paymentChannel>
                <fee>1.50</fee>
                <primaryCiIncurredFee>0.50</primaryCiIncurredFee>
                <idBundle>1</idBundle>
                <idCiBundle>2</idCiBundle>
                <payer>
                    <uniqueIdentifier>
                        <entityUniqueIdentifierType>F</entityUniqueIdentifierType>
                        <entityUniqueIdentifierValue>${bundle.debtor.fiscalCode}</entityUniqueIdentifierValue>
                    </uniqueIdentifier>
                    <fullName>${bundle.debtor.fullName}</fullName>
                    <streetName>${bundle.debtor.streetName}</streetName>
                    <civicNumber>${bundle.debtor.civicNumber}</civicNumber>
                    <postalCode>${bundle.debtor.postalCode}</postalCode>
                    <city>${bundle.debtor.city}</city>
                    <stateProvinceRegion>${bundle.debtor.province}</stateProvinceRegion>
                    <country>${bundle.debtor.country}</country>
                    <e-mail>${bundle.debtor.email}</e-mail>
                </payer>
                <applicationDate>${bundle.debtPositionCalculation.applicationDate}</applicationDate>
                <transferDate>${bundle.debtPositionCalculation.transferDate}</transferDate>
            </details>
            </nod:sendPaymentOutcomeReq>
        </soapenv:Body>
    </soapenv:Envelope>`;
}

function buildSendRTRequest(bundle) {
    return `<?xml version="1.0" encoding="utf-8"?>
        <Envelope xmlns="http://schemas.xmlsoap.org/soap/envelope/">
            <Body>
                <paSendRTReq xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns="http://pagopa-api.pagopa.gov.it/pa/paForNode.xsd">
                    <idPA xmlns="">${bundle.creditorInstitution.id}</idPA>
                    <idBrokerPA xmlns="">${bundle.broker.id}</idBrokerPA>
                    <idStation xmlns="">${bundle.station.id}</idStation>
                    <receipt xmlns="">
                        <receiptId>${bundle.debtPositionCalculation.paymentToken}</receiptId>
                        <noticeNumber>${bundle.debtPositionCalculation.noticeNumber}</noticeNumber>
                        <fiscalCode>${bundle.creditorInstitution.id}</fiscalCode>
                        <outcome>OK</outcome>
                        <creditorReferenceId>${bundle.debtPosition.paymentOption[0].iuv}</creditorReferenceId>
                        <paymentAmount>${bundle.debtPositionCalculation.decimalAmount}</paymentAmount>
                        <description>Pagamento ${bundle.debtPosition.paymentOption[0].description}</description>
                        <companyName>${bundle.debtor.companyName}</companyName>
                        <officeName>${bundle.debtor.officeName}</officeName>
                        <debtor>
                            <uniqueIdentifier>
                                <entityUniqueIdentifierType>F</entityUniqueIdentifierType>
                                <entityUniqueIdentifierValue>${bundle.debtor.fiscalCode}</entityUniqueIdentifierValue>
                            </uniqueIdentifier>
                            <fullName>${bundle.debtor.fullName}</fullName>
                            <streetName>${bundle.debtor.streetName}</streetName>
                            <civicNumber>${bundle.debtor.civicNumber}</civicNumber>
                            <postalCode>${bundle.debtor.postalCode}</postalCode>
                            <city>${bundle.debtor.city}</city>
                            <stateProvinceRegion>${bundle.debtor.province}</stateProvinceRegion>
                            <country>${bundle.debtor.country}</country>
                            <e-mail>${bundle.debtor.email}</e-mail>
                        </debtor>
                        <transferList>
                            <transfer>
                                <idTransfer>1</idTransfer>
                                <transferAmount>${bundle.debtPosition.paymentOption[0].transfer[0].amount}.00</transferAmount>
                                <fiscalCodePA>${bundle.debtor.fiscalCode}</fiscalCodePA>
                                <IBAN>${bundle.debtPosition.paymentOption[0].transfer[0].iban}</IBAN>
                                <remittanceInformation>${bundle.debtPosition.paymentOption[0].transfer[0].remittanceInformation}</remittanceInformation>
                                <transferCategory>G</transferCategory>
                            </transfer>
                            <transfer>
                                <idTransfer>2</idTransfer>
                                <transferAmount>${bundle.debtPosition.paymentOption[0].transfer[1].amount}.00</transferAmount>
                                <fiscalCodePA>${bundle.debtor.fiscalCode}</fiscalCodePA>
                                <IBAN>${bundle.debtPosition.paymentOption[0].transfer[1].iban}</IBAN>
                                <remittanceInformation>${bundle.debtPosition.paymentOption[0].transfer[1].remittanceInformation}</remittanceInformation>
                                <transferCategory>G</transferCategory>
                            </transfer>
                        </transferList>
                        <idPSP>${bundle.psp.id}</idPSP>
                        <pspFiscalCode>${bundle.psp.fiscalCode}</pspFiscalCode>
                        <PSPCompanyName>${bundle.psp.businessName}</PSPCompanyName>
                        <idChannel>${bundle.psp.channelId}</idChannel>
                        <channelDescription>app</channelDescription>
                        <payer>
                            <uniqueIdentifier>
                                <entityUniqueIdentifierType>F</entityUniqueIdentifierType>
                                <entityUniqueIdentifierValue>${bundle.debtor.fiscalCode}</entityUniqueIdentifierValue>
                            </uniqueIdentifier>
                            <fullName>${bundle.debtor.fullName}</fullName>
                            <streetName>${bundle.debtor.streetName}</streetName>
                            <civicNumber>${bundle.debtor.civicNumber}</civicNumber>
                            <postalCode>${bundle.debtor.postalCode}</postalCode>
                            <city>${bundle.debtor.city}</city>
                            <stateProvinceRegion>${bundle.debtor.province}</stateProvinceRegion>
                            <country>${bundle.debtor.country}</country>
                            <e-mail>${bundle.debtor.email}</e-mail>
                        </payer>
                        <paymentMethod>creditCard</paymentMethod>
                        <fee>1.50</fee>
                        <paymentDateTime>${bundle.debtPositionCalculation.applicationDate}T12:00:00</paymentDateTime>
                        <applicationDate>${bundle.debtPositionCalculation.applicationDate}</applicationDate>
                        <transferDate>${bundle.debtPositionCalculation.transferDate}</transferDate>
                    </receipt>
                </paSendRTReq>
            </Body>
        </Envelope>`;
}

function buildVerifyPaymentNoticeRequest(bundle) {
  return `<?xml version="1.0" encoding="utf-8"?>
         <Envelope xmlns="http://schemas.xmlsoap.org/soap/envelope/">
             <Body>
                 <paVerifyPaymentNoticeReq xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns="http://pagopa-api.pagopa.gov.it/pa/paForNode.xsd">
                     <idPA xmlns="">${bundle.creditorInstitution.id}</idPA>
                     <idBrokerPA xmlns="">${bundle.broker.id}</idBrokerPA>
                     <idStation xmlns="">${bundle.station.id}</idStation>
                     <qrCode xmlns="">
                         <fiscalCode>${bundle.creditorInstitution.id}</fiscalCode>
                         <noticeNumber>${bundle.debtPositionCalculation.noticeNumber}</noticeNumber>
                     </qrCode>
                 </paVerifyPaymentNoticeReq>
             </Body>
         </Envelope>`;
}

module.exports = {
    buildActivatePaymentNoticeRequest,
    buildReportFlowCreationRequest,
    buildReportFlowsRetrieveRequest,
    buildReportFlowForDebtPositionRequest,
    buildSendPaymentOutcomeRequest,
    buildSendRTRequest,
    buildVerifyPaymentNoticeRequest,
};
