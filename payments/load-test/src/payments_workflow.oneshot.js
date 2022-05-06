import http from 'k6/http';
import {check, sleep} from 'k6';
import {parseHTML} from "k6/html";
import Papa from "./modules/papaparse.min.js";


export let options = {
    vus: 1,
    iterations: 1,
    summaryTrendStats: ['avg', 'min', 'med', 'max', 'p(95)', 'p(99)', 'p(99.99)', 'count'],
    // stages: [
    // 	{ duration: '1m', target: 50 }, // simulate ramp-up of traffic from 1 to 50 users over 1 minutes.
    // ],
    thresholds: {
        http_req_failed: ['rate<0.01'], // http errors should be less than 1%
        http_req_duration: ['p(99)<1500'], // 99% of requests must complete below 1500ms
        'http_req_duration{gpdMethod:CreateDebtPosition}': ['p(95)<1000'], // threshold on creation API requests only
        'http_req_duration{gpdMethod:PublishDebtPosition}': ['p(95)<1000'], // threshold on publication API requests only
        'http_req_duration{paymentRequest:VerifyPayment}': ['p(95)<1000'], // threshold on payment API requests only
        'http_req_duration{paymentRequest:GetPayment}': ['p(95)<1000'], // threshold on report API requests only
        'http_req_duration{paymentRequest:SendRT}': ['p(95)<1000'], // threshold on get API requests only
    },

};

const filename = `${__ENV.FILENAME}`;
const data = open(filename);

export function setup() {
    return Papa.parse(data, {header: true});
}


var urlPaymentsBasePath = `${__ENV.BASE_PAYMENTS_URL}`
var idBrokerPA = `${__ENV.ID_BROKER_PA}`
var idStation = `${__ENV.ID_STATION}`
var service = `${__ENV.LOCAL}`.toLowerCase() === "yes" ? "partner" : ""

export default function (results) {
    for (let row of results.data) {
        // console.log(JSON.stringify(row));
        if (row.hasOwnProperty("pa_id_istat")) {
            callPayments(row.pa_id_istat, row.payment_notice_number, row.amount, `payment_token_${row.id}`, row.debtor_name, row.debtor_email, row.debtor_id_fiscal_code);
        }
    }

}

function callPayments(creditor_institution_code, notice_number, amount, receiptId, debtorName, debtorMail, fiscalcode) {
    // Verify Payment.
    let tag = {
        paymentRequest: "VerifyPayment",
    };

    let url = `${urlPaymentsBasePath}/${service}`;

    let payload = `<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/" xmlns:nod="http://pagopa-api.pagopa.gov.it/pa/paForNode.xsd">
    <soapenv:Header />
    <soapenv:Body>
        <nod:paVerifyPaymentNoticeReq>
            <idPA>${creditor_institution_code}</idPA>
            <idBrokerPA>${idBrokerPA}</idBrokerPA>
            <idStation>${idStation}</idStation>
            <qrCode>
                <fiscalCode>${creditor_institution_code}</fiscalCode>
                <noticeNumber>${notice_number}</noticeNumber>
            </qrCode>
        </nod:paVerifyPaymentNoticeReq>
    </soapenv:Body>
</soapenv:Envelope>`;


    let params = {
        headers: {
            'Content-Type': 'text/xml',
            'SOAPAction': 'paVerifyPaymentNotice'
        },
    };
    console.log(JSON.stringify(payload));
    let r = http.post(url, payload, params);
    console.log(JSON.stringify(r));
    console.log("VerifyPayment req - creditor_institution_code = " + creditor_institution_code + ", iuv = " + notice_number + ", Status = " + r.status);
    if (r.status != 200 && r.status != 504) {
        console.error("-> VerifyPayment req - creditor_institution_code = " + creditor_institution_code + ", iuv = " + notice_number + ", Status = " + r.status + ", Body=" + r.body);
    }

    check(r, {
        'VerifyPayment status is 200 and outcome is OK': (r) => r.status === 200 && (parseHTML(r.body)).find('outcome').get(0).textContent() === 'OK',
    }, tag);


    // if the verify payment has OK => activate payment
    if (r.status === 200 && parseHTML(r.body).find('outcome').get(0).textContent() === 'OK') {
        sleep(4);
        // Activate Payment.
        tag = {
            paymentRequest: "GetPayment",
        };

        url = `${urlPaymentsBasePath}/${service}`;

        payload = `<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/" xmlns:pafn="http://pagopa-api.pagopa.gov.it/pa/paForNode.xsd">
    <soapenv:Header />
    <soapenv:Body>
        <pafn:paGetPaymentReq>
            <idPA>${creditor_institution_code}</idPA>
            <idBrokerPA>${idBrokerPA}</idBrokerPA>
            <idStation>${idStation}</idStation>
            <qrCode>
                <fiscalCode>${creditor_institution_code}</fiscalCode>
                <noticeNumber>${notice_number}</noticeNumber>
            </qrCode>
            <amount>${amount}</amount>
        </pafn:paGetPaymentReq>
    </soapenv:Body>
</soapenv:Envelope>`;

        params = {
            headers: {
                'Content-Type': 'text/xml',
                'SOAPAction': 'paGetPayment'
            },
        };

        r = http.post(url, payload, params);

        console.log("GetPayment req - creditor_institution_code = " + creditor_institution_code + ", iuv = " + notice_number + ", Status = " + r.status);
        if (r.status != 200 && r.status != 504) {
            console.error("-> GetPayment req - creditor_institution_code = " + creditor_institution_code + ", iuv = " + notice_number + ", Status = " + r.status + ", Body=" + r.body);
        }

        check(r, {
            'ActivatePayment status is 200 and outcome is OK': (r) => r.status === 200 && (parseHTML(r.body)).find('outcome').get(0).textContent() === 'OK',
        }, tag);

        // if the activate payment has been OK => send receipt
        if (r.status === 200 && parseHTML(r.body).find('outcome').get(0).textContent() === 'OK') {
            sleep(8);
            // Get details of a specific payment option.
            tag = {
                paymentRequest: "SendRT",
            };

            url = `${urlPaymentsBasePath}/${service}`;

            payload = `<soapenv:Envelope xmlns:pafn="http://pagopa-api.pagopa.gov.it/pa/paForNode.xsd" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xs="http://www.w3.org/2001/XMLSchema" xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/">
    <soapenv:Body>
        <pafn:paSendRTReq>
            <idPA>${creditor_institution_code}</idPA>
            <idBrokerPA>${idBrokerPA}</idBrokerPA>
            <idStation>${idStation}</idStation>
            <receipt>
                <receiptId>${receiptId}</receiptId>
                <noticeNumber>${notice_number}</noticeNumber>
                <fiscalCode>${creditor_institution_code}</fiscalCode>
                <outcome>OK</outcome>
                <creditorReferenceId>${notice_number}</creditorReferenceId>
                <paymentAmount>${amount}</paymentAmount>
                <description>test</description>
                <companyName>company Name</companyName>
                <officeName>office Name</officeName>
                <debtor>
                    <uniqueIdentifier>
                        <entityUniqueIdentifierType>F</entityUniqueIdentifierType>
                        <entityUniqueIdentifierValue>${fiscalcode}</entityUniqueIdentifierValue>
                    </uniqueIdentifier>
                    <fullName>${debtorName}</fullName>
                    <streetName>via roma</streetName>
                    <civicNumber>1</civicNumber>
                    <postalCode>00111</postalCode>
                    <city>rome</city>
                    <stateProvinceRegion>MI</stateProvinceRegion>
                    <country>IT</country>
                    <e-mail>${debtorMail}</e-mail>
                </debtor>
                <transferList>
                    <transfer>
                        <idTransfer>1</idTransfer>
                        <transferAmount>${amount}</transferAmount>
                        <fiscalCodePA>${creditor_institution_code}</fiscalCodePA>
                        <IBAN>IT23X0000100001000000000999</IBAN>
                        <remittanceInformation>remittanceInformation1</remittanceInformation>
                        <transferCategory>G</transferCategory>
                    </transfer>
                </transferList>
                <idPSP>88888888888</idPSP>
                <pspFiscalCode>88888888888</pspFiscalCode>
                <pspPartitaIVA>88888888888</pspPartitaIVA>
                <PSPCompanyName>PSP name</PSPCompanyName>
                <idChannel>88888888888_01</idChannel>
                <channelDescription>app</channelDescription>
                <payer>
                    <uniqueIdentifier>
                        <entityUniqueIdentifierType>F</entityUniqueIdentifierType>
                        <entityUniqueIdentifierValue>JHNDOE00A01F205N</entityUniqueIdentifierValue>
                    </uniqueIdentifier>
                    <fullName>John Doe</fullName>
                    <streetName>street</streetName>
                    <civicNumber>12</civicNumber>
                    <postalCode>89020</postalCode>
                    <city>city</city>
                    <stateProvinceRegion>MI</stateProvinceRegion>
                    <country>IT</country>
                    <e-mail>john.doe@test.it</e-mail>
                </payer>
                <paymentMethod>creditCard</paymentMethod>
                <fee>2.00</fee>
                <paymentDateTime>2021-10-01T17:48:22</paymentDateTime>
                <applicationDate>2021-10-01</applicationDate>
                <transferDate>2021-10-02</transferDate>
            </receipt>
        </pafn:paSendRTReq>
    </soapenv:Body>
</soapenv:Envelope>`;

            params = {
                headers: {
                    'Content-Type': 'text/xml',
                    'SOAPAction': 'paSendRT'
                },
            };

            r = http.post(url, payload, params);

            console.log("SendRT req - creditor_institution_code = " + creditor_institution_code + ", iuv = " + notice_number + ", Status = " + r.status);
            if (r.status != 200 && r.status != 504) {
                console.error("-> SendRT req - creditor_institution_code = " + creditor_institution_code + ", iuv = " + notice_number + ", Status = " + r.status + ", Body=" + r.body);
            }

            check(r, {
                'SendRT status is 200 and outcome is OK': (r) => r.status === 200 && (parseHTML(r.body)).find('outcome').get(0).textContent() === 'OK',
            }, tag);

        }
    }
}

