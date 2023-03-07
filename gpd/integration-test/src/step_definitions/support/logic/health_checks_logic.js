const assert = require("assert");
const { gpdHealthCheck } = require("../clients/gpd_client");

async function executeHealthCheckForGPD() {
    const response = await gpdHealthCheck();
    assert.strictEqual(response.status, 200);
}

module.exports = {
    executeHealthCheckForGPD
}