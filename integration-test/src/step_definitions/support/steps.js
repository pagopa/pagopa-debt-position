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
        executeMassiveDebtPositionCreationWithSegregationCodes
} = require('./logic/gpd_logic');
const { assertAmount, assertFaultCode, assertOutcome, assertStatusCode, assertCompanyName, assertNotificationFeeUpdatedAmounts, 
assertStatusString, executeAfterAllStep, randomOrg, randomIupd, assertIupd, assertNav, assertNotificationFeeUpdatedDateNotificationFee, assertSize, assertMinSize,
    assertIUV
} = require('./logic/common_logic');
const { gpdSessionBundle, gpdUpdateBundle, gpdPayBundle } = require('./utility/data');
const { getValidBundle, addDays, format } = require('./utility/helpers');



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

BeforeAll(async function() {
    await executeDebtPositionDeletion(gpdSessionBundle, idOrg, iupdOK);
    await executeDebtPositionDeletion(gpdSessionBundle, idOrg, iupdKO);
 });

/*
 *  'Given' precondition for health checks on various services.
 */
Given('GPD running', () => executeHealthCheckForGPD());

/*
 *  Debt position creation
 */
Given('a random iupd', async function () {
    iupd = randomIupd();
    // precondition -> deletion possible dirty data
    await executeDebtPositionDeletion(gpdSessionBundle, idOrg, iupd);
    });
When(/^the debt position with IUPD (.*) and payment option with IUV (.*) is created$/, (IUPD_input, iuv) => executeDebtPositionCreation(gpdSessionBundle, idOrg, IUPD_input, iuv));
When('the debt position is created', () => executeDebtPositionCreation(gpdSessionBundle, idOrg, iupd));
Then('the debt position gets the status code {int}', (statusCode) => assertStatusCode(gpdSessionBundle, statusCode));
Then('the organization gets the nav value after creation', () => assertNav(gpdSessionBundle.createdDebtPosition, gpdSessionBundle.responseToCheck.data));

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
When('we ask the list of organizations debt positions', async () => {
    await executeDebtPositionGetList(gpdSessionBundle, idOrg, dueDateFrom, dueDateTo, paymentDateFrom, paymentDateTo, status)
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
When('the debt position is updated', () => executeDebtPositionUpdate(gpdSessionBundle, gpdUpdateBundle, idOrg, iupd));
Then('the organization gets the update status code {int}', (statusCode) => assertStatusCode(gpdSessionBundle, statusCode));
Then('the organization gets the nav value after update', () => assertNav(gpdSessionBundle.updatedDebtPosition, gpdSessionBundle.responseToCheck.data));


/*
 *  Debt position get
 */
When('we get the debt position', () => executeDebtPositionGet(gpdSessionBundle, idOrg, iupd));
When(/^we get the debt position by IUV (.*)$/, (iuv) => executeDebtPositionGetByIuv(gpdSessionBundle, idOrg, iuv));
Then('the company name is {string}', (companyName) => assertCompanyName(gpdSessionBundle, companyName));
Then('the organization get the nav value', () => assertNav(gpdSessionBundle.responseToCheck.data, gpdSessionBundle.responseToCheck.data))
Then(/^the debt position response IUV value is (.*)$/, (expectedIUV) => assertIUV(gpdSessionBundle.responseToCheck.data, expectedIUV))

/*
 *  Debt position delete
 */
When('the debt position is deleted', () => executeDebtPositionDeletion(gpdSessionBundle, idOrg, iupd));
Then(/^the debt position with IUPD (.*) is deleted$/, (IUPD_input) => executeDebtPositionDeletion(gpdSessionBundle, idOrg, IUPD_input));

/*
 *  Debt position publish
 */
When('the debt position is published', () => executeDebtPositionPublish(gpdSessionBundle, idOrg, iupd));
When('the debt position using segregation codes is published', () => executeDebtPositionPublishWithSegregationCodes(gpdSessionBundle, idOrg, iupd));
Then('the organization gets the nav value after publication', () => assertNav(gpdSessionBundle.createdDebtPosition, gpdSessionBundle.responseToCheck.data))

/*
 *  Debt position invalidate
 */
 
 When('the debt position using segregation codes is invalidated', () => executeDebtPositionInvalidateWithSegregationCodes(gpdSessionBundle, idOrg, iupd));

/*
 *  Paying the payment option
 */
When('the payment option is paid', () => executePaymentOptionPay(gpdPayBundle, idOrg, gpdSessionBundle.debtPosition.iuv1));
Then('the payment option gets the status code {int}', (statusCode) => assertStatusCode(gpdSessionBundle, statusCode));

/*
 *  Payment Option get by IUV
 */
When('we get the payment option by iuv', () => executePaymentOptionGetByIuv(gpdSessionBundle, idOrg, gpdSessionBundle.debtPosition.iuv1));
Then('the get payment options returns the status code {int}', (statusCode) => assertStatusCode(gpdSessionBundle, statusCode));
Then('the response returns the status code {int}', (statusCode) => assertStatusCode(gpdSessionBundle, statusCode));
Then('the iupd is present and valued with the same value as the debt position', () => assertIupd(gpdSessionBundle));

/*
 *  Reporting the transfer
 */
When('the transfer is reported', () => executeReportTransfer(gpdSessionBundle, idOrg));
Then('the transfer gets the status code {int}', (statusCode) => assertStatusCode(gpdSessionBundle, statusCode));

/*
 *  Create and publish
 */
When('the debt position is created and published', () => executeDebtPositionCreationAndPublication(gpdSessionBundle, idOrg, iupd));
Then('the debt position gets status {string}', (statusString) => assertStatusString(gpdSessionBundle, statusString));

Given('a new debt position', () => executeDebtPositionCreation(gpdSessionBundle, idOrg, iupd));
When('the debt position is updated and published', () => executeDebtPositionUpdateAndPublication(gpdSessionBundle, idOrg, iupd));

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