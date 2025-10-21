const assert = require("assert");
const {toLog} = require("../utility/helpers");

const auxDigit = process.env.aux_digit

async function assertAmount(bundle, amount) {
    await assertOutcome(bundle, "OK");
    await assert.match(bundle.responseToCheck.data, new RegExp(`<amount>${amount}</amount>`, "g"));
}

async function assertFaultCode(bundle, fault) {
    await assertOutcome(bundle, "KO");
    await assert.match(bundle.responseToCheck.data, new RegExp(`<faultCode>${fault}</faultCode>`, "g"));
}

async function assertOutcome(bundle, outcome) {
    assert.match(bundle.responseToCheck.data, new RegExp(`<outcome>${outcome}</outcome>`, "g"));
}

async function assertStatusCode(bundle, statusCode) {
    if(bundle.responseToCheck.status != statusCode) {
        toLog("[assertStatusCode] executed request: " + JSON.stringify(bundle.responseToCheck.request.path))
        toLog("[assertStatusCode] failed response: " + JSON.stringify(bundle.responseToCheck.data))
    }
    assert.strictEqual(bundle.responseToCheck.status, statusCode);
}

async function assertCompanyName(bundle, companyName) {
    assert.strictEqual(bundle.payer.companyName, companyName);
}

async function assertDebtPositionStatus(bundle, debtPositionStatus) {
    assert.strictEqual(bundle.debtPositionStatus, debtPositionStatus);
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

async function assertIUV(response, expectedIUV) {
    assert.strictEqual(response.paymentOption[0].iuv, expectedIUV);
}

async function assertSize (array, expectedSize){
	assert.strictEqual(array.length, expectedSize)
}

async function assertMinSize (array, minSize){
	assert.ok(array.length >= minSize)
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
    assertDebtPositionStatus,
    assertNotificationFeeUpdatedAmounts,
    assertStatusString,
    randomOrg,
    randomIupd,
    assertIupd,
    assertNav,
    assertIUV,
    assertNotificationFeeUpdatedDateNotificationFee,
    assertSize,
    assertMinSize
}