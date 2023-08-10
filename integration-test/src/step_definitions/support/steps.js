const { Given, When, Then, AfterAll, Before} = require('@cucumber/cucumber')
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
        executePaymentOptionGetByIuv
} = require('./logic/gpd_logic');
const { assertAmount, assertFaultCode, assertOutcome, assertStatusCode, assertCompanyName, assertNotificationFeeUpdatedAmounts, 
assertStatusString, executeAfterAllStep, randomOrg, randomIupd, assertIupd, assertNav } = require('./logic/common_logic');
const { gpdSessionBundle, gpdUpdateBundle, gpdPayBundle } = require('./utility/data');
const { getValidBundle, addDays, format } = require('./utility/helpers');

let idOrg = process.env.organization_fiscal_code;
let iupd;
let status;
let dueDateFrom;
let dueDateTo;
let paymentDateFrom;
let paymentDateTo;

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
When('the debt position is created', () => executeDebtPositionCreation(gpdSessionBundle, idOrg, iupd, status));
Then('the debt position gets the status code {int}', (statusCode) => assertStatusCode(gpdSessionBundle, statusCode));
Then('the organization gets the nav value after creation', () => assertNav(gpdSessionBundle.createdDebtPosition, gpdSessionBundle.responseToCheck.data));

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
 *  Debt position update
 */
When('the debt position is updated', () => executeDebtPositionUpdate(gpdSessionBundle, gpdUpdateBundle, idOrg, iupd));
Then('the organization gets the update status code {int}', (statusCode) => assertStatusCode(gpdSessionBundle, statusCode));
Then('the organization gets the nav value after update', () => assertNav(gpdSessionBundle.updatedDebtPosition, gpdSessionBundle.responseToCheck.data));


/*
 *  Debt position get
 */
When('we get the debt position', () => executeDebtPositionGet(gpdSessionBundle, idOrg, iupd));
Then('the company name is {string}', (companyName) => assertCompanyName(gpdSessionBundle, companyName));
Then('the organization get the nav value', () => assertNav(gpdSessionBundle.responseToCheck.data, gpdSessionBundle.responseToCheck.data))

/*
 *  Debt position delete
 */
When('the debt position is deleted', () => executeDebtPositionDeletion(gpdSessionBundle, idOrg, iupd));


/*
 *  Debt position publish
 */
When('the debt position is published', () => executeDebtPositionPublish(gpdSessionBundle, idOrg, iupd));
Then('the organization gets the nav value after publication', () => assertNav(gpdSessionBundle.createdDebtPosition, gpdSessionBundle.responseToCheck.data))

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

Given('a new debt position', () => executeDebtPositionCreation(gpdSessionBundle, idOrg, iupd, status));
When('the debt position is updated and published', () => executeDebtPositionUpdateAndPublication(gpdSessionBundle, idOrg, iupd));

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
});