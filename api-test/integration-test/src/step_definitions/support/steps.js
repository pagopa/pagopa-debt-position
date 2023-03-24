const { Before, BeforeStep, Given, setDefaultTimeout, Then, When } = require('@cucumber/cucumber');
const { 
    assertEmptyList,
    assertFlowXMLContent,
    assertNonEmptyList,
    assertPaymentOptionStatus,
    assertStatusCode
} = require('./logic/common_logic');
const { 
    generateAndPayDebtPosition, 
    generateDebtPosition,
    retrievePaymentOptionDetail
} = require('./logic/gpd_logic');
const { 
    executeHealthCheckForAPIConfig, 
    executeHealthCheckForGPD, 
    executeHealthCheckForGPDPayments,
    executeHealthCheckForReportingAnalysis, 
} = require('./logic/health_checks_logic');
const { 
    forceReportingBatchStart,
    retrieveReportFlowList,
    retrieveReportFlow,
    sendReportFlowToNode, 
    waitWholeReportingProcessExecution,
} = require('./logic/reporting_logic');
const { bundle } = require('./utility/data');

/* Setting defaul timeout to 10s. */
setDefaultTimeout(90 * 1000);


/* 
 *  'Given' precondition for health checks on various services. 
 */
Given('GPD service running', () => executeHealthCheckForGPD());
Given('APIConfig service running', () => executeHealthCheckForAPIConfig());
Given('GPD Payments service running', () => executeHealthCheckForGPDPayments());
Given('reporting analysis service running', () => executeHealthCheckForReportingAnalysis());


/* 
 *  'Given' precondition for validating the entities to be used. 
 */
Given('a not paid debt position', () => generateDebtPosition(bundle, true));
Given('a paid debt position', () => generateAndPayDebtPosition(bundle));
Given('a report flow sent to Node', () => sendReportFlowToNode(bundle));


/* 
 *  'When' clauses for executing actions.
 */
When('the reporting batch analyzes the reporting flows for the organization', () => forceReportingBatchStart(bundle));
When('the client waits its execution', () => waitWholeReportingProcessExecution());


/* 
 *  'Then' clauses for executing subsequential actions
 */
Then('the client asks the flow list for the organization', () => retrieveReportFlowList(bundle));
Then('the client asks the detail for one of the report flows', () => retrieveReportFlow(bundle));
Then('the client asks the detail for the analyzed debt positions', () => retrievePaymentOptionDetail(bundle));


/* 
 *  'Then' clauses for assering retrieved data 
 */
Then('the client receives status code {int}', (statusCode) => assertStatusCode(bundle.response, statusCode));
Then('the client receives a non-empty list of flows', () => assertNonEmptyList(bundle.response));
Then('the client receives an empty list of flows', () => assertEmptyList(bundle.response));
Then('the client receives the flow XML content', () => assertFlowXMLContent(bundle.response));
Then('the client receives the payment options with status {string}', (status) => assertPaymentOptionStatus(bundle.response, status));


Before(function(scenario) {
    const header = `| Starting scenario "${scenario.pickle.name}" |`;
    let h = "-".repeat(header.length);
    console.log(`\n${h}`);
    console.log(`${header}`);
    console.log(`${h}`);
});