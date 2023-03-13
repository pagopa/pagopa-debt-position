const assert = require("assert");

async function assertAmount(bundle, amount) {
    assertOutcome(bundle, "OK");
    assert.match(bundle.responseToCheck.data, new RegExp(`<amount>${amount}</amount>`, "g"));
}

async function assertFaultCode(bundle, fault) {
    assertOutcome(bundle, "KO");
    assert.match(bundle.responseToCheck.data, new RegExp(`<faultCode>${fault}</faultCode>`, "g"));
}

async function assertOutcome(bundle, outcome) {
    assert.match(bundle.responseToCheck.data, new RegExp(`<outcome>${outcome}</outcome>`, "g"));
}

async function assertStatusCode(bundle, statusCode) {
    assert.strictEqual(bundle.responseToCheck.status, statusCode);
}

async function assertCompanyName(bundle, companyName) {
    assert.strictEqual(bundle.payer.companyName, companyName);
}

async function assertStatusString(bundle, statusString) {
    assert.strictEqual(bundle.responseToCheck.data.status, statusString);
}

function randomOrg() {
    return "Org_" + Math.floor(Math.random() * 100);
}

function randomIupd() {
    return "IUPD_" + Math.floor(Math.random() * 100);
}

module.exports = {
    assertAmount,
    assertFaultCode,
    assertOutcome,
    assertStatusCode,
    assertCompanyName,
    assertStatusString,
    randomOrg,
    randomIupd,
}