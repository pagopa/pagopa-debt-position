import string
import random
import base64


def get_random_string(length):
    # choose from all lowercase letter
    nums = string.digits
    result_str = ''.join(random.choice(nums) for i in range(length))
    return result_str

def nodoChiediElencoFlussiRendicontazioneF(idFlusso1, idFlusso2):
    return f'''<?xml version="1.0" encoding="UTF-8" standalone="no"?>
        <soapenv:Envelope xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/" xmlns:ppt="http://ws.pagamenti.telematici.gov/" xmlns:tns="http://NodoPagamentiSPC.spcoop.gov.it/servizi/PagamentiTelematiciRPT" xmlns:ppthead="http://ws.pagamenti.telematici.gov/ppthead">
            <soapenv:Body>
                <ppt:nodoChiediElencoFlussiRendicontazioneRisposta>
                    <elencoFlussiRendicontazione>
                        <totRestituiti>2</totRestituiti>
                        <idRendicontazione>
                            <identificativoFlusso>2022-01-12PPAYITR1XXX-S{idFlusso1}</identificativoFlusso>
                            <dataOraFlusso>2022-01-12T00:31:05</dataOraFlusso>
                        </idRendicontazione>
                        <idRendicontazione>
                            <identificativoFlusso>2022-01-20PPAYITR1XXX-S{idFlusso2}</identificativoFlusso>
                            <dataOraFlusso>2022-01-20T00:30:39</dataOraFlusso>
                        </idRendicontazione>
                    </elencoFlussiRendicontazione>
                </ppt:nodoChiediElencoFlussiRendicontazioneRisposta>
            </soapenv:Body>
        </soapenv:Envelope>
        '''


def howManyDatiSingoliPagamenti(n, data):
    payments = ""
    for i in range(n):
        singlePayment=f'''<datiSingoliPagamenti>
            <identificativoUnivocoVersamento>{get_random_string(17)}</identificativoUnivocoVersamento>
            <identificativoUnivocoRiscossione>IUR{get_random_string(17)}</identificativoUnivocoRiscossione>
            <indiceDatiSingoloPagamento>{i+1}</indiceDatiSingoloPagamento>
            <singoloImportoPagato>100.00</singoloImportoPagato>
            <codiceEsitoSingoloPagamento>0</codiceEsitoSingoloPagamento>
            <dataEsitoSingoloPagamento>{data}</dataEsitoSingoloPagamento>
        </datiSingoliPagamenti>'''
        payments+=singlePayment+"\n"

    return payments

def generateXmlFlusso(identificativoFlusso,dataOraFlusso,dataRegolamento,istitutoMittente):
    flusso = f'''<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
            <FlussoRiversamento xmlns="http://www.digitpa.gov.it/schemas/2011/Pagamenti/">
                <versioneOggetto>1.0</versioneOggetto>
                <identificativoFlusso>{identificativoFlusso}</identificativoFlusso>
                <dataOraFlusso>{dataOraFlusso}</dataOraFlusso>
                <identificativoUnivocoRegolamento>Bonifico SEPA-{get_random_string(5)}-77777777777</identificativoUnivocoRegolamento>
                <dataRegolamento>{dataRegolamento}</dataRegolamento>
                <istitutoMittente>
                    <identificativoUnivocoMittente>
                        <tipoIdentificativoUnivoco>B</tipoIdentificativoUnivoco>
                        <codiceIdentificativoUnivoco>{istitutoMittente}</codiceIdentificativoUnivoco>
                    </identificativoUnivocoMittente>
                    <denominazioneMittente>AGID</denominazioneMittente>
                </istitutoMittente>
                <istitutoRicevente>
                    <identificativoUnivocoRicevente>
                        <tipoIdentificativoUnivoco>G</tipoIdentificativoUnivoco>
                        <codiceIdentificativoUnivoco>77777777777</codiceIdentificativoUnivoco>
                    </identificativoUnivocoRicevente>
                    <denominazioneRicevente>AGSM ENERGIA S.R.L. SOCIETA' UNIPERSONAL E</denominazioneRicevente>
                </istitutoRicevente>
                <numeroTotalePagamenti>{2}</numeroTotalePagamenti>
                <importoTotalePagamenti>${2*100}.00</importoTotalePagamenti>
                {howManyDatiSingoliPagamenti(2,"2022-02-02")}
            </FlussoRiversamento>
    '''
    return base64.b64encode(bytes(flusso, 'utf-8')).decode('utf-8')


def nodoChiediFlussoRendicontazioneF(idFlusso):
    return f'''<?xml version="1.0" encoding="UTF-8" standalone="no"?>
        <soapenv:Envelope xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/" xmlns:ppt="http://ws.pagamenti.telematici.gov/" xmlns:tns="http://NodoPagamentiSPC.spcoop.gov.it/servizi/PagamentiTelematiciRPT" xmlns:ppthead="http://ws.pagamenti.telematici.gov/ppthead">
            <soapenv:Body>
                <ppt:nodoChiediFlussoRendicontazioneRisposta>
                    <xmlRendicontazione>{generateXmlFlusso(idFlusso,"2022-02-02","2022-02-02","78787878787")}</xmlRendicontazione>
                </ppt:nodoChiediFlussoRendicontazioneRisposta>
            </soapenv:Body>
        </soapenv:Envelope>
        '''
