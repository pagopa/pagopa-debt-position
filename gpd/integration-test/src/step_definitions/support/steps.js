const { Given, When, Then, AfterAll, Before} = require('@cucumber/cucumber')
const { executeHealthCheckForGPD } = require('./logic/health_checks_logic');
const { executeDebtPositionCreation,
        executeDebtPositionDeletion,
        executeDebtPositionGetList,
        executeDebtPositionUpdate,
        executeDebtPositionGet,
        executeDebtPositionPublish,
        executePaymentOptionPay,
        executeReportTransfer,
        executeDebtPositionCreationAndPublication,
} = require('./logic/gpd_logic');
const { assertAmount, assertFaultCode, assertOutcome, assertStatusCode, assertCompanyName, assertStatusString, executeAfterAllStep, randomOrg, randomIupd } = require('./logic/common_logic');
const { gpdSessionBundle, gpdUpdateBundle, gpdPayBundle } = require('./utility/data');
const { getValidBundle, addDays, format } = require('./utility/helpers');

let idOrg;
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
Given('a random organization id and iupd', async function () {
    idOrg = randomOrg();
    iupd = randomIupd();
    // precondition -> deletion possible dirty data
    await executeDebtPositionDeletion(idOrg, iupd);
    });
When('the debt position is created', () => executeDebtPositionCreation(gpdSessionBundle, idOrg, iupd, status));
Then('the debt position gets the status code {int}', (statusCode) => assertStatusCode(gpdSessionBundle, statusCode));

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
 *  Debt position update
 */
When('the debt position is updated', () => executeDebtPositionUpdate(gpdUpdateBundle, idOrg, iupd));
Then('the organization gets the status code {int}', (statusCode) => assertStatusCode(gpdUpdateBundle, statusCode));

/*
 *  Debt position get
 */
When('we get the debt position', () => executeDebtPositionGet(gpdSessionBundle, idOrg, iupd));
Then('the company name is {string}', (companyName) => assertCompanyName(gpdSessionBundle, companyName));

/*
 *  Debt position delete
 */
When('the debt position is deleted', () => executeDebtPositionDeletion(gpdSessionBundle, idOrg, iupd));


/*
 *  Debt position publish
 */
When('the debt position is published', () => executeDebtPositionPublish(gpdSessionBundle, idOrg, iupd));

/*
 *  Paying the payment option
 */
When('the payment option is paid', () => executePaymentOptionPay(gpdPayBundle, idOrg, gpdSessionBundle.debtPosition.iuv1));
Then('the payment option gets the status code {int}', (statusCode) => assertStatusCode(gpdSessionBundle, statusCode));

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

function resetParams() {
    dueDateFrom = null;
    dueDateTo = null;
    paymentDateFrom = null;
    paymentDateTo = null;
    status = null;
}