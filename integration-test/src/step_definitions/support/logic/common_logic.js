const assert = require("assert");

const auxDigit = process.env.aux_digit

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

async function assertIupd(bundle) {
  assert.strictEqual(bundle.debtPosition.iupd, bundle.responseToCheck.data.iupd)
}

async function assertStatusString(bundle, statusString) {
    assert.strictEqual(bundle.responseToCheck.data.status, statusString);
}

async function assertNotificationFeeUpdatedAmounts(createdDebtPosition, response) {
    let notificationFee = response.notificationFee;
    assert.strictEqual(response.amount, createdDebtPosition.paymentOption[0].amount + notificationFee);
    assert.strictEqual(response.transfer[0].amount, createdDebtPosition.paymentOption[0].transfer[0].amount + notificationFee);
    assert.strictEqual(response.transfer[1].amount, createdDebtPosition.paymentOption[0].transfer[1].amount);
}

async function assertNotificationFeeUpdatedDateNotificationFee(createdDebtPosition, response) {
	let lastUpdatedDateNotificationFee = response.lastUpdatedDateNotificationFee;
	assert.notEqual(lastUpdatedDateNotificationFee, null);
}

async function assertNav(debtPosition, response) {
    // nav = auxDigit + iuv
    assert.strictEqual(response.paymentOption[0].nav, auxDigit + debtPosition.paymentOption[0].iuv);
}

function randomOrg() {
    return "Org_" + Math.floor(Math.random() * 100);
}

function randomIupd() {
    return "IUPD_" + Math.floor(Math.random() * 1000000);
}

module.exports = {
    assertAmount,
    assertFaultCode,
    assertOutcome,
    assertStatusCode,
    assertCompanyName,
    assertNotificationFeeUpdatedAmounts,
    assertStatusString,
    randomOrg,
    randomIupd,
    assertIupd,
    assertNav,
    assertNotificationFeeUpdatedDateNotificationFee
}