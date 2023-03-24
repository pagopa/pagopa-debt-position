const assert = require("assert");

async function assertEmptyList(response) {
    console.log(` - -> the client receives an empty list of flows..`);
    assert.ok(response.data.length === 0);
}

async function assertFlowXMLContent(response) {
    console.log(` - -> the client receives the flow XML content with outcome [OK]..`);
    assert.match(response.data, new RegExp(`<esito>OK</esito>`, "g"));
    assert.match(response.data, new RegExp(`<xmlRendicontazione>.+</xmlRendicontazione>`, "g"));    
}

async function assertNonEmptyList(response) {
    console.log(` - -> the client receives a non-empty list of flows..`);
    assert.ok(response.data.length > 0);
}

async function assertPaymentOptionStatus(response, status) {
    console.log(` - -> the client receives the payment options with status [${status}]..`);
    assert.strictEqual(response.data.status, status);
}

async function assertStatusCode(response, statusCode) {
    console.log(` - -> the client receives status code [${statusCode}]..`);
    assert.strictEqual(response.status, statusCode);
}

module.exports = {
    assertEmptyList,
    assertFlowXMLContent,
    assertNonEmptyList,
    assertPaymentOptionStatus,
    assertStatusCode,
}