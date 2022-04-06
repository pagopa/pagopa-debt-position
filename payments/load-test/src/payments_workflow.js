import http from 'k6/http';
import { check } from 'k6';
import { parseHTML } from "k6/html";
import { sleep } from 'k6';
import { makeidNumber, makeidMix, randomString } from './modules/helpers.js';

export let options = {
	summaryTrendStats: ['avg', 'min', 'med', 'max', 'p(95)', 'p(99)', 'p(99.99)', 'count'],
	stages: [
		{ duration: '1m', target: 50 }, // simulate ramp-up of traffic from 1 to 50 users over 1 minutes.
	],
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

export default function() {

	var urlGPDBasePath = `${__ENV.BASE_GPD_URL}`
	var urlPaymentsBasePath = `${__ENV.BASE_PAYMENTS_URL}`
	var idBrokerPA = `${__ENV.ID_BROKER_PA}`
	var idStation = `${__ENV.ID_STATION}`
	var service = `${__ENV.LOCAL}`.toLowerCase() === "yes" ? "partner" : ""

	const creditor_institution_code = randomString(11, "0123456789");
	const iupd = makeidMix(35);
	const iuv_1 = makeidNumber(17);
	const iuv_2 = makeidNumber(17);
	const iuv_3 = makeidNumber(17);
	const due_date = new Date().addDays(30);
	const retention_date = new Date().addDays(90);
	const transfer_id_1 = '1';
	const transfer_id_2 = '2';
	const receiptId = makeidMix(33);

	// Create new debt position (no validity date).
	var tag = {
		gpdMethod: "CreateDebtPosition",
	};


	var url = `${urlGPDBasePath}/organizations/${creditor_institution_code}/debtpositions`;

	var payload = JSON.stringify(
		{
			"iupd": iupd,
			"type": "F",
			"fiscalCode": "JHNDOE00A01F205N",
			"fullName": "John Doe",
			"streetName": "streetName",
			"civicNumber": "11",
			"postalCode": "00100",
			"city": "city",
			"province": "RM",
			"region": "RM",
			"country": "IT",
			"email": "lorem@lorem.com",
			"phone": "333-123456789",
			"companyName": "companyName",
			"officeName": "officeName",
			"paymentOption": [
				{
					"iuv": iuv_1,
					"amount": 10000,
					"description": "Canone Unico Patrimoniale - CORPORATE opt 1 FINAL",
					"isPartialPayment": false,
					"dueDate": due_date,
					"retentionDate": retention_date,
					"fee": 0,
					"transfer": [
						{
							"idTransfer": transfer_id_1,
							"amount": 9000,
							"remittanceInformation": "remittanceInformation 1",
							"category": "9/0101108TS/",
							"iban": "IT0000000000000000000000000"
						},
						{
							"idTransfer": transfer_id_2,
							"amount": 1000,
							"remittanceInformation": "remittanceInformation 2",
							"category": "9/0101108TS/",
							"iban": "IT0000000000000000000000000"
						}
					]
				},
				{
					"iuv": iuv_2,
					"amount": 5000,
					"description": "Canone Unico Patrimoniale - CORPORATE opt 2 NOT FINAL",
					"isPartialPayment": true,
					"dueDate": due_date,
					"retentionDate": retention_date,
					"fee": 0,
					"transfer": [
						{
							"idTransfer": transfer_id_1,
							"amount": 4000,
							"remittanceInformation": "remittanceInformation 1",
							"category": "9/0101108TS/",
							"iban": "IT0000000000000000000000000"
						},
						{
							"idTransfer": transfer_id_2,
							"amount": 1000,
							"remittanceInformation": "remittanceInformation 2",
							"category": "9/0101108TS/",
							"iban": "IT0000000000000000000000000"
						}
					]
				},
				{
					"iuv": iuv_3,
					"amount": 5000,
					"description": "Canone Unico Patrimoniale - CORPORATE opt 3 NOT FINAL",
					"isPartialPayment": true,
					"dueDate": due_date,
					"retentionDate": retention_date,
					"fee": 0,
					"transfer": [
						{
							"idTransfer": transfer_id_1,
							"amount": 4000,
							"remittanceInformation": "remittanceInformation 1",
							"category": "9/0101108TS/",
							"iban": "IT0000000000000000000000000"
						},
						{
							"idTransfer": transfer_id_2,
							"amount": 1000,
							"remittanceInformation": "remittanceInformation 2",
							"category": "9/0101108TS/",
							"iban": "IT0000000000000000000000000"
						}
					]
				}
			]
		});


	var params = {
		headers: {
			'Content-Type': 'application/json'
		},
	};

	var r = http.post(url, payload, params);

	console.log("CreateDebtPosition call - creditor_institution_code = " + creditor_institution_code + ", Status = " + r.status);

	check(r, {
		'CreateDebtPosition status is 201': (r) => r.status === 201,
	}, tag);

	// if the debt position has been correctly created => publish 
	if (r.status === 201) {
		// Publish the debt position.
		tag = {
			gpdMethod: "PublishDebtPosition",
		};
		url = `${urlGPDBasePath}/organizations/${creditor_institution_code}/debtpositions/${iupd}/publish`;

		r = http.post(url, params);

		console.log("PublishDebtPosition call - creditor_institution_code = " + creditor_institution_code + ", iupd = " + iupd + ", Status = " + r.status);

		check(r, {
			'PublishDebtPosition status is 200': (r) => r.status === 200,
		}, tag);

		// if the debt position has been correctly published => verify payment 
		if (r.status === 200) {
			sleep(2);
			// Verify Payment.
			tag = {
				paymentRequest: "VerifyPayment",
			};

			url = `${urlPaymentsBasePath}/${service}`;

			payload = `<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/" xmlns:nod="http://pagopa-api.pagopa.gov.it/pa/paForNode.xsd">
    <soapenv:Header />
    <soapenv:Body>
        <nod:paVerifyPaymentNoticeReq>
            <idPA>${creditor_institution_code}</idPA>
            <idBrokerPA>${idBrokerPA}</idBrokerPA>
            <idStation>${idStation}</idStation>
            <qrCode>
                <fiscalCode>${creditor_institution_code}</fiscalCode>
                <noticeNumber>3${iuv_1}</noticeNumber>
            </qrCode>
        </nod:paVerifyPaymentNoticeReq>
    </soapenv:Body>
</soapenv:Envelope>`;


			params = {
				headers: {
					'Content-Type': 'text/xml',
					'SOAPAction': 'paVerifyPaymentNotice'
				},
			};

			r = http.post(url, payload, params);

            console.log("VerifyPayment req - creditor_institution_code = " + creditor_institution_code + ", iuv = " + iuv_1 + ", Status = " + r.status);
            if (r.status != 200 && r.status != 504) {
                console.error("-> VerifyPayment req - creditor_institution_code = " + creditor_institution_code + ", iuv = " + iuv_1 + ", Status = " + r.status + ", Body=" + r.body);
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
                <noticeNumber>3${iuv_1}</noticeNumber>
            </qrCode>
            <amount>10.00</amount>
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

                console.log("GetPayment req - creditor_institution_code = " + creditor_institution_code + ", iuv = " + iuv_1 + ", Status = " + r.status);
				if (r.status != 200 && r.status != 504) {
                    console.error("-> GetPayment req - creditor_institution_code = " + creditor_institution_code + ", iuv = " + iuv_1 + ", Status = " + r.status + ", Body=" + r.body);
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
                <noticeNumber>3${iuv_1}</noticeNumber>
                <fiscalCode>${creditor_institution_code}</fiscalCode>
                <outcome>OK</outcome>
                <creditorReferenceId>${iuv_1}</creditorReferenceId>
                <paymentAmount>30.00</paymentAmount>
                <description>test</description>
                <companyName>company EC</companyName>
                <officeName>office EC</officeName>
                <debtor>
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
                </debtor>
                <transferList>
                    <transfer>
                        <idTransfer>1</idTransfer>
                        <transferAmount>20.00</transferAmount>
                        <fiscalCodePA>${creditor_institution_code}</fiscalCodePA>
                        <IBAN>IT0000000000000000000000000</IBAN>
                        <remittanceInformation>remittanceInformation1</remittanceInformation>
                        <transferCategory>G</transferCategory>
                    </transfer>
                    <transfer>
                        <idTransfer>2</idTransfer>
                        <transferAmount>10.00</transferAmount>
                        <fiscalCodePA>${creditor_institution_code}</fiscalCodePA>
                        <IBAN>IT0000000000000000000000001</IBAN>
                        <remittanceInformation>remittanceInformation2</remittanceInformation>
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
                <metadata>
                    <mapEntry>
                        <key>keytest</key>
                        <value>1</value>
                    </mapEntry>
                </metadata>
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

					console.log("SendRT req - creditor_institution_code = " + creditor_institution_code + ", iuv = " + iuv_1 + ", Status = " + r.status);
                    if (r.status != 200 && r.status != 504) {
                        console.error("-> SendRT req - creditor_institution_code = " + creditor_institution_code + ", iuv = " + iuv_1 + ", Status = " + r.status + ", Body=" + r.body);
                    }

					check(r, {
						'SendRT status is 200 and outcome is OK': (r) => r.status === 200 && (parseHTML(r.body)).find('outcome').get(0).textContent() === 'OK',
					}, tag);

				}
			}
		}

	}
}
