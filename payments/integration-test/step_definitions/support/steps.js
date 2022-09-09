const {Given, When, Then, After, AfterAll} = require('@cucumber/cucumber')
const {healthCheck, demandPaymentNotice} = require("./payments_client");
const {gpsHealthCheck, createOrganization, deleteOrganization, createService, deleteService} = require("./gps_client");
const {iuvGenHealthCheck} = require("./iuv_generator_client");
const {donationHealthCheck} = require("./donation_service_client");
const {apiConfigHealthCheck} = require("./api_config_client");
const {gpdHealthCheck} = require("./gpd_client");
const assert = require("assert");
const fs = require("fs");

let rawdata = fs.readFileSync('./config/properties.json');
let properties = JSON.parse(rawdata);
const donation_host = properties.donation_host;

let responseToCheck;
let serviceCode;
let organizationCode;

Given('Payments running', async function () {
    const response = await healthCheck();
    assert.strictEqual(response.status, 200);
});
Given('GPS running', async function () {
    const response = await gpsHealthCheck();
    assert.strictEqual(response.status, 200);
});
Given('GPD running', async function () {
    const response = await gpdHealthCheck();
    assert.strictEqual(response.status, 200);
});
Given('IUV Generator running', async function () {
	//TODO expose health check on APIM
    //const response = await iuvGenHealthCheck();
    //assert.strictEqual(response.status, 200);
});
Given('DonationService running', async function () {
	//TODO expose health check on APIM
    //const response = await donationHealthCheck();
    //assert.strictEqual(response.status, 200);
});
Given('ApiConfig running', async function () {
    const response = await apiConfigHealthCheck();
    assert.strictEqual(response.status, 200);
});

Given('the service {string} for donations', async function (serviceId) {
    serviceCode = serviceId;
    await deleteService(serviceCode)
    let response = await createService({
        "id": serviceId,
        "name": "DonationpagoPAservice",
        "description": "DonationpagoPAservice",
        "transferCategory": "tassonomia-1",
        "status": "ENABLED",
        "endpoint": donation_host,
        "basePath": "/donations/paymentoptions",
        "properties": [
            {
                "name": "amount",
                "type": "NUMBER",
                "required": true
            },
            {
                "name": "description",
                "type": "STRING"
            }
        ]
    })
    assert.strictEqual(response.status, 201);
});
Given('the creditor institution {string} enrolled to donation service {string}', async function (orgId, serviceId) {
    organizationCode = orgId
    await deleteOrganization(organizationCode)
    let response = await createOrganization(orgId, {
        "companyName": "Comune di Milano",
        "enrollments": [
            {
                "serviceId": serviceId,
                "iban": "IT00000000000000001",
                "officeName": "Ufficio Tributi",
                "segregationCode": "77",
                "remittanceInformation": "causale di pagamento"
            }
        ]
    })
    
    assert.strictEqual(response.status, 201);
});
When('the client sends the DemandPaymentNoticeRequest', async function () {
    responseToCheck = await demandPaymentNotice(`<soapenv:Envelope xmlns:pafn="http://pagopa-api.pagopa.gov.it/pa/paForNode.xsd" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xs="http://www.w3.org/2001/XMLSchema" xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/">
        <soapenv:Body>
            <pafn:paDemandPaymentNoticeRequest>
                <idPA>${organizationCode}</idPA>
                <idBrokerPA>15376371009</idBrokerPA>
                <idStation>15376371009_01</idStation>
                <idServizio>12345</idServizio>
                <datiSpecificiServizioRequest>PHNlcnZpY2UgeG1sbnM9Imh0dHA6Ly9QdW50b0FjY2Vzc29QU1Auc3Bjb29wLmdvdi5pdC9HZW5lcmFsU2VydmljZSIgeHNpOnNjaGVtYUxvY2F0aW9uPSJodHRwOi8vUHVudG9BY2Nlc3NvUFNQLnNwY29vcC5nb3YuaXQvR2VuZXJhbFNlcnZpY2Ugc2NoZW1hLnhzZCIgeG1sbnM6eHNpPSJodHRwOi8vd3d3LnczLm9yZy8yMDAxL1hNTFNjaGVtYS1pbnN0YW5jZSI+CiAgPGFtb3VudD4xMDA8L2Ftb3VudD4KICA8ZGVzY3JpcHRpb24+ZG9uYXRpb248L2Rlc2NyaXB0aW9uPgo8L3NlcnZpY2U+</datiSpecificiServizioRequest>
            </pafn:paDemandPaymentNoticeRequest>
        </soapenv:Body>
    </soapenv:Envelope>`);
});
When(/^the client sends a wrong DemandPaymentNoticeRequest$/, async function () {
    responseToCheck = await demandPaymentNotice(`<soapenv:Envelope xmlns:pafn="http://pagopa-api.pagopa.gov.it/pa/paForNode.xsd" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xs="http://www.w3.org/2001/XMLSchema" xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/">
        <soapenv:Body>
            <pafn:paDemandPaymentNoticeRequest>
                <idPA>${organizationCode}</idPA>
                <idBrokerPA>15376371009</idBrokerPA>
                <idStation>15376371009_01</idStation>
                <idServizio>12345</idServizio>
                <datiSpecificiServizioRequest>PHNlcnZpY2UgeG1sbnM9Imh0dHA6Ly9QdW50b0FjY2Vzc29QU1Auc3Bjb29wLmdvdi5pdC9HZW5lcmFsU2VydmljZSIgeHNpOnNjaGVtYUxvY2F0aW9uPSJodHRwOi8vUHVudG9BY2Nlc3NvUFNQLnNwY29vcC5nb3YuaXQvR2VuZXJhbFNlcnZpY2Ugc2NoZW1hLnhzZCIgeG1sbnM6eHNpPSJodHRwOi8vd3d3LnczLm9yZy8yMDAxL1hNTFNjaGVtYS1pbnN0YW5jZSI+CiAgPGRlc2NyaXB0aW9uPmRvbmF0aW9uPC9kZXNjcmlwdGlvbj4KPC9zZXJ2aWNlPg==</datiSpecificiServizioRequest>
            </pafn:paDemandPaymentNoticeRequest>
        </soapenv:Body>
    </soapenv:Envelope>`);
});
Then('the client receives status code {int}', function (statusCode) {
    assert.strictEqual(responseToCheck.status, statusCode);
});
Then('the client retrieves the amount in the response', function () {
    assert.match(responseToCheck.data, /<outcome>OK<\/outcome>/);
    assert.match(responseToCheck.data, /<amount>1.00<\/amount>/);
});
Then(/^the client receives a KO in the response$/, function () {
    assert.match(responseToCheck.data, /<outcome>KO<\/outcome>/);
});


// Asynchronous Promise
AfterAll(function () {
    deleteService(serviceCode)
    deleteOrganization(organizationCode)
});
