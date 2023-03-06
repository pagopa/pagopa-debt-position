const {Given, When, Then, AfterAll, Before} = require('@cucumber/cucumber')
const { executeHealthCheckForGPD } = require('./logic/health_checks_logic');
const { executeDebtPositionCreation } = require('./logic/gpd_logic');
const { assertAmount, assertFaultCode, assertOutcome, assertStatusCode, executeAfterAllStep } = require('./logic/common_logic');
const { createOrganizationInfo, createServiceInfo, sendInvalidDemandPaymentNoticeRequest, sendValidDemandPaymentNoticeRequest } = require('./logic/gps_logic');
const { gpdSessionBundle, gpsSessionBundle } = require('./utility/data');
const { getValidBundle } = require('./utility/helpers');



/*
 *  'Given' precondition for health checks on various services.
 */
Given('GPD running', () => executeHealthCheckForGPD());


/*
 *  Debt position creation
 */
Given('a random organization id {string}', (orgId) => readCreditorInstitutionInfo(gpdSessionBundle, orgId));
When('the debt position is created', () => sendVerifyPaymentNoticeRequest(gpdSessionBundle));
Then('the debt position gets the status code {int}', (statusCode) => assertStatusCode(getValidBundle(gpdSessionBundle, gpsSessionBundle), statusCode));
