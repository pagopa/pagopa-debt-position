const {Given, When, Then} = require('@cucumber/cucumber')
const {healthCheck, demandPaymentNotice} = require("./payments_client");
const {gpsHealthCheck, createOrganization, deleteOrganization, createService, deleteService} = require("./gps_client");
const assert = require("assert");
const fs = require("fs");

let rawdata = fs.readFileSync('./config/properties.json');
let properties = JSON.parse(rawdata);
const donation_host = properties.donation_host;

let responseToCheck;

Given('Payments running', async function () {
    const response = await healthCheck();
    assert.strictEqual(response.status, 200);
});
Given('GPS running', async function () {
    const response = await gpsHealthCheck();
    assert.strictEqual(response.status, 200);
});
Given('DonationService running', async function () {
    // TODO call donation health check
});
Given('the service {string} for donations', function (serviceId) {
    let response = createService(serviceId, {
        "id":serviceId,
        "name":"DonationpagoPAservice",
        "description":"DonationpagoPAservice",
        "transferCategory":"tassonomia-1",
        "status":"ENABLED",
        "endpoint":donation_host,
        "basePath":"/donations/paymentoptions",
        "properties":[
            {
                "name":"amount",
                "type":"NUMBER",
                "required":true
            },
            {
                "name":"description",
                "type":"STRING"
            }
        ]
    })
    assert.strictEqual(response.status, 201);
});
Given('the creditor institution {string} enrolled to donation service {string}', function (orgId, serviceId) {
    let response = createOrganization(orgId, {
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
    responseToCheck = await demandPaymentNotice(<soapenv:Envelope xmlns:pafn="http://pagopa-api.pagopa.gov.it/pa/paForNode.xsd" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xs="http://www.w3.org/2001/XMLSchema" xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/">
        <soapenv:Body>
            <pafn:paDemandPaymentNoticeRequest>
                <idPA>77777777777</idPA>
                <idBrokerPA>15376371009</idBrokerPA>
                <idStation>15376371009_01</idStation>
                <idServizio>12345</idServizio>
                <datiSpecificiServizioRequest>PHNlcnZpY2UgeG1sbnM9Imh0dHA6Ly9QdW50b0FjY2Vzc29QU1Auc3Bjb29wLmdvdi5pdC9HZW5lcmFsU2VydmljZSIgeHNpOnNjaGVtYUxvY2F0aW9uPSJodHRwOi8vUHVudG9BY2Nlc3NvUFNQLnNwY29vcC5nb3YuaXQvR2VuZXJhbFNlcnZpY2Ugc2NoZW1hLnhzZCIgeG1sbnM6eHNpPSJodHRwOi8vd3d3LnczLm9yZy8yMDAxL1hNTFNjaGVtYS1pbnN0YW5jZSI+CiAgPGFtb3VudD4xMDA8L2Ftb3VudD4KICA8ZGVzY3JpcHRpb24+ZG9uYXRpb248L2Rlc2NyaXB0aW9uPgo8L3NlcnZpY2U+</datiSpecificiServizioRequest>
            </pafn:paDemandPaymentNoticeRequest>
        </soapenv:Body>
    </soapenv:Envelope>);
});
Then('the client receives status code {int}', function (statusCode) {
    assert.strictEqual(responseToCheck.status, statusCode);
});
Then('the client retrieves the amount in the response', function () {
    assert.strictEqual(responseToCheck.data, 0); // TODO
});
