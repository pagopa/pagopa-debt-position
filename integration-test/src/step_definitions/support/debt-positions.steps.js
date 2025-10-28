const { Given, When, Then, AfterAll, Before, BeforeAll, setDefaultTimeout} = require('@cucumber/cucumber')
const { executeHealthCheckForGPD } = require('./logic/health_checks_logic');
const { executeDebtPositionCreation,
        executeDebtPositionDeletion,
        executeDebtPositionGetList,
        executeDebtPositionNotificationFeeUpdate,
        executeDebtPositionUpdate,
        executeDebtPositionGet,
        executeDebtPositionPublish,
        executePaymentOptionPay,
        executeReportTransfer,
        executeDebtPositionCreationAndPublication,
        executeDebtPositionUpdateAndPublication,
        executePaymentOptionGetByIuv,
        executeDebtPositionGetByIuv,
        executeOKDebtPositionCreation,
        executeKODebtPositionCreation,
        executeDebtPositionNotificationFeeUpdateNodeOK,
        executeDebtPositionNotificationFeeUpdateNodeKO,
        executeDebtPositionCreationWithSegregationCodes,
        executeDebtPositionUpdateWithSegregationCodes,
        executeDebtPositionGetWithSegregationCodes,
        executeDebtPositionGetListWithSegregationCodes,
        executeDebtPositionDeletionWithSegregationCodes,
        executeDebtPositionPublishWithSegregationCodes,
        executeDebtPositionInvalidateWithSegregationCodes,
        executeMassiveDebtPositionsCreation,
        executeMassiveDebtPositionCreationWithSegregationCodes,
        executeUpdateTransferIbanMassive
} = require('./logic/gpd_logic');
const { assertAmount, assertFaultCode, assertOutcome, assertStatusCode, assertCompanyName, assertNotificationFeeUpdatedAmounts, 
assertStatusString, executeAfterAllStep, randomOrg, randomIupd, assertIupd, assertNav, assertNotificationFeeUpdatedDateNotificationFee, assertSize, assertMinSize,
    assertIUV, assertDebtPositionStatus
} = require('./logic/common_logic');
const { gpdSessionBundle, gpdUpdateBundle, gpdPayBundle } = require('./utility/data');
const { getValidBundle, addDays, addSeconds, format, makeidNumber} = require('./utility/helpers');
const assert = require("assert");



// increase cucumber promise timeout
setDefaultTimeout(30000);

let idOrg = process.env.organization_fiscal_code;
let iupd;
let iupdOK = "iupdOK";
let iupdKO = "iupdKO";
let status;
let dueDateFrom;
let dueDateTo;
let paymentDateFrom;
let paymentDateTo;
let responseToCheck;

BeforeAll(async function() {
    await executeDebtPositionDeletion(gpdSessionBundle, idOrg, iupdOK);
    await executeDebtPositionDeletion(gpdSessionBundle, idOrg, iupdKO);
 });

/*
 *  Debt position creation
 */
Given('a random iupd', async function () {
    iupd = randomIupd();
    gpdSessionBundle.debtPosition.iupd = iupd;
    // precondition -> deletion possible dirty data
    await executeDebtPositionDeletion(gpdSessionBundle, idOrg, iupd);
});
When(/^the debt position with IUPD (.*) and payment option with IUV (.*) is created$/, (IUPD_input, iuv) => executeDebtPositionCreation(gpdSessionBundle, idOrg, IUPD_input, version = "v1", iuv));
When('the debt position is created using {string} API', (version) => executeDebtPositionCreation(gpdSessionBundle, idOrg, iupd, version));
When('the debt position with validityDate in {int} seconds is created', async (seconds) => {
    await executeDebtPositionCreation(gpdSessionBundle, idOrg, iupd, version = "v1", iuv = makeidNumber(17), validityDate = addSeconds(seconds), toPublish = true);
});
Then('the debt position gets the status code {int}', (statusCode) => assertStatusCode(gpdSessionBundle, statusCode));
Then('the organization gets the nav value from {string} after creation', (fromComponent) => assertNav(gpdSessionBundle.createdDebtPosition, gpdSessionBundle.responseToCheck.data, fromComponent));

/*
 *  Massive debt positions creation
 */

When('the debt position items is created', () => executeMassiveDebtPositionsCreation(gpdSessionBundle, idOrg, iupd));


/*
 * Node OK and KO result Debt position creation
 */
When('a node OK result debt position is created', () => executeOKDebtPositionCreation(gpdSessionBundle, idOrg, iupdOK));
When('a node KO result debt position is created', () => executeKODebtPositionCreation(gpdSessionBundle, idOrg, iupdKO));

/*
 *  Debt position list
 */
Given('the filter made by status {string}', (statusParam) => status = statusParam);
Given('the filter made by due date from today to {int} days', (daysParam) => {
    dueDateFrom = format(new Date());
    dueDateTo = format(addDays(daysParam));
});
Given('the filter made by payment date from today to {int} days', (daysParam) => {
    paymentDateFrom = format(new Date());
    paymentDateTo = format(addDays(daysParam));
});
When('we ask the list of organizations debt positions using {string} API', async (version) => {
    await executeDebtPositionGetList(gpdSessionBundle, idOrg, dueDateFrom, dueDateTo, paymentDateFrom, paymentDateTo, status, version = version); 
    resetParams();
});
Then('we get the status code {int}', (statusCode) => assertStatusCode(gpdSessionBundle, statusCode));

/*
 *  Debt position notification fee update
 */
When('the notification fee of the debt position is updated', () => executeDebtPositionNotificationFeeUpdate(gpdSessionBundle, idOrg, 150));
Then('the organization gets the status code {int}', (statusCode) => assertStatusCode(gpdSessionBundle, statusCode));
Then('the organization gets the updated amounts', () => assertNotificationFeeUpdatedAmounts(gpdSessionBundle.createdDebtPosition, gpdSessionBundle.responseToCheck.data));

/*
 * Debt position notification fee update by querying the node with existing positions
 */
 When('the notification fee of the debt position is updated using an OK position on the node', () => executeDebtPositionNotificationFeeUpdateNodeOK(gpdSessionBundle, idOrg, 150));
 When('the notification fee of the debt position is updated using an KO position on the node', () => executeDebtPositionNotificationFeeUpdateNodeKO(gpdSessionBundle, idOrg, 150));
 Then('the organization gets the updated last updated date notification fee', () => assertNotificationFeeUpdatedDateNotificationFee(gpdSessionBundle.createdDebtPosition, 
 gpdSessionBundle.responseToCheck.data));
 

/*
 *  Debt position update
 */
When('the debt position is updated using {string} API', (version) => executeDebtPositionUpdate(gpdSessionBundle, gpdUpdateBundle, idOrg, iupd, version = version));
Then('the organization gets the update status code {int}', (statusCode) => assertStatusCode(gpdSessionBundle, statusCode));
Then('the organization gets the nav value from {string} after update', (fromComponent) => assertNav(gpdSessionBundle.updatedDebtPosition, gpdSessionBundle.responseToCheck.data, fromComponent));


/*
 *  Debt position get
 */
When('we get the debt position using {string} API', (version) => executeDebtPositionGet(gpdSessionBundle, idOrg, iupd, version = version));
When(/^we get the debt position by IUV (.*)$/, (iuv) => executeDebtPositionGetByIuv(gpdSessionBundle, idOrg, iuv));
Then('the company name is {string}', (companyName) => assertCompanyName(gpdSessionBundle, companyName));
Then('the organization get the nav value from {string}', (fromComponent) => assertNav(gpdSessionBundle.responseToCheck.data, gpdSessionBundle.responseToCheck.data, fromComponent))
Then(/^the debt position response IUV value is (.*)$/, (expectedIUV) => assertIUV(gpdSessionBundle.responseToCheck.data, expectedIUV))

/*
 *  Debt position delete
 */
When('the debt position is deleted using {string} API', (version) => executeDebtPositionDeletion(gpdSessionBundle, idOrg, iupd, version = version));
Then(/^the debt position with IUPD (.*) is deleted$/, (IUPD_input) => executeDebtPositionDeletion(gpdSessionBundle, idOrg, IUPD_input));

/*
 *  Debt position publish
 */
When('the debt position is published using {string} API', (version) => executeDebtPositionPublish(gpdSessionBundle, idOrg, iupd, version = version));
When('the debt position using segregation codes is published', () => executeDebtPositionPublishWithSegregationCodes(gpdSessionBundle, idOrg, iupd));
Then('the organization gets the nav value from {string} after publication', (fromComponent) => assertNav(gpdSessionBundle.createdDebtPosition, gpdSessionBundle.responseToCheck.data, fromComponent))

/*
 *  Debt position invalidate
 */
 
 When('the debt position using segregation codes is invalidated', () => executeDebtPositionInvalidateWithSegregationCodes(gpdSessionBundle, idOrg, iupd));

/*
 *  Paying the payment option
 */
When('the payment option is paid', async () => await executePaymentOptionPay(gpdPayBundle, idOrg, gpdSessionBundle.debtPosition.iuv1));
Then('the payment option gets the status code {int}', (statusCode) => assertStatusCode(gpdPayBundle, statusCode));

/*
 *  Payment Option get by IUV
 */
When('we get the payment option by iuv', () => executePaymentOptionGetByIuv(gpdSessionBundle, idOrg, gpdSessionBundle.debtPosition.iuv1));
Then('the get payment options returns the status code {int}', (statusCode) => assertStatusCode(gpdSessionBundle, statusCode));
Then('the response returns the status code {int}', (statusCode) => assertStatusCode(gpdSessionBundle, statusCode));
Then('the iupd is present and valued with the same value as the debt position', () => assertIupd(gpdSessionBundle));
Then('the debt position is in the status {string}', (status) => assertDebtPositionStatus(gpdSessionBundle.responseToCheck.data, status));

/*
 *  Reporting the transfer
 */
When('the transfer is reported', () => executeReportTransfer(gpdSessionBundle, idOrg));
Then('the transfer gets the status code {int}', (statusCode) => assertStatusCode(gpdSessionBundle, statusCode));

/*
 *  Create and publish
 */
When('the debt position is created and published using {string} API', (version) => executeDebtPositionCreationAndPublication(gpdSessionBundle, idOrg, iupd, version));
Then('the debt position gets status {string}', (statusString) => assertStatusString(gpdSessionBundle, statusString));

When('the debt position is updated and published using {string} API', (version) => executeDebtPositionUpdateAndPublication(gpdSessionBundle, idOrg, iupd, version));

/*
 * Debt position manage with segregation codes
 */
 
 When('the debt position using segregation codes is created', () => executeDebtPositionCreationWithSegregationCodes(gpdSessionBundle, idOrg, iupd));
 When('the debt position items, using segregation codes, is created', () => executeMassiveDebtPositionCreationWithSegregationCodes(gpdSessionBundle, idOrg, iupd, status));
 When('the debt position using segregation codes is updated', () => executeDebtPositionUpdateWithSegregationCodes(gpdSessionBundle, gpdUpdateBundle, idOrg, iupd));
 When('the organization gets the debt position using segregation codes', () => executeDebtPositionGetWithSegregationCodes(gpdSessionBundle, idOrg, iupd));
 When('the debt position using segregation codes is deleted', () => executeDebtPositionDeletionWithSegregationCodes(gpdSessionBundle, idOrg, iupd));
 When('the organization gets the list of debt positions using segregation codes', {timeout: 20000}, async () => {
    await executeDebtPositionGetListWithSegregationCodes(gpdSessionBundle, idOrg)
    resetParams();});
 Then('the debt positions list size is greater than {int}', (size) => assertMinSize(gpdSessionBundle.responseToCheck.data.payment_position_list, size));

/*
 * Update IBAN on all Organization's Transfers
 */
When('the updateTransferIbanMassive is called with oldIban {string} and newIban {string}', async (oldIban, newIban) => responseToCheck = await executeUpdateTransferIbanMassive(idOrg, oldIban, newIban));
Then('the updateTransferIbanMassive gets the status code {int}', (statusCode) => assert.strictEqual(responseToCheck.status, statusCode));
Then('the updateTransferIbanMassive response includes number of updates', () => assert.ok(responseToCheck.data.updatedTransfers >= 0));

 /*
 * Utility steps
 */

 When('system wait {int} seconds', async (seconds) => {
    await wait(seconds * 1000); // Convert seconds to milliseconds
});

function wait(ms) {
    return new Promise(resolve => setTimeout(resolve, ms));
}
 
function resetParams() {
    dueDateFrom = null;
    dueDateTo = null;
    paymentDateFrom = null;
    paymentDateTo = null;
    status = null;
}

AfterAll(async function() {
    // postcondition -> deletion possible duplication
    await executeDebtPositionDeletion(gpdSessionBundle, idOrg, iupd);
    await executeDebtPositionDeletion(gpdSessionBundle, idOrg, iupdOK);
    await executeDebtPositionDeletion(gpdSessionBundle, idOrg, iupdKO);
});