const {Given, When, Then} = require('@cucumber/cucumber')
const {healthCheck, demandPaymentNotice} = require("./payments_client");
const {gpsHealthCheck, createOrganization, deleteOrganization, createService, deleteOrganization} = require("./gps_client");
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
    responseToCheck = await demandPaymentNotice({
        // TODO
    });
});
Then('the client receives status code {int}', function (statusCode) {
    assert.strictEqual(responseToCheck.status, statusCode);
});
Then('the client retrieves the amount in the response', function () {
    assert.strictEqual(responseToCheck.data, 0); // TODO
});
