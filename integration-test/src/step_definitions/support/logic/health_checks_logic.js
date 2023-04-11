const assert = require("assert");
const { apiConfigHealthCheck } = require("../clients/api_config_client");
const { gpdHealthCheck } = require("../clients/gpd_client");
const { paymentsHealthCheck } = require("../clients/payments_client");
const { getFlowList } = require("../clients/reporting_client");
const { debugLog } = require("../utility/helpers");


async function executeHealthCheckForAPIConfig() {
    console.log(" - Given APIConfig service running...");
    const response = await apiConfigHealthCheck();
    debugLog(`APIConfig Health check API invocation returned HTTP status code: ${response?.status}`);
    assert.strictEqual(response.status, 200);
}

async function executeHealthCheckForGPD() {
    console.log(" - Given GPD service running...");
    const response = await gpdHealthCheck();
    debugLog(`GPD Health check API invocation returned HTTP status code: ${response?.status}`);
    assert.strictEqual(response.status, 200);
}

async function executeHealthCheckForGPDPayments() {
    console.log(" - Given GPD Payments service running...");
    const response = await paymentsHealthCheck();
    debugLog(`GPD Payments Health check API invocation returned HTTP status code: ${response?.status}`);
    assert.strictEqual(response.status, 200);
}

async function executeHealthCheckForReportingAnalysis() {
    console.log(" - Given reporting analysis service running...");
    const response = await getFlowList("00000000000");
    debugLog(`Reporting analysis Health check API invocation returned HTTP status code: ${response?.status}`);
    assert.strictEqual(response.status, 200);
}

module.exports = {
    executeHealthCheckForAPIConfig,
    executeHealthCheckForGPD,
    executeHealthCheckForGPDPayments,
    executeHealthCheckForReportingAnalysis,
}