const {
    createDebtPosition,
    publishDebtPosition
} = require("../clients/gpd_client");

const {
    buildDebtPositionDynamicData
} = require("../utility/request_builders");

async function executeDebtPositionCreation(bundle) {
    bundle.debtPosition = buildDebtPositionDynamicData(bundle);
    let response = await createDebtPosition(bundle.organizationCode, buildCreateDebtPositionRequest(bundle.debtPosition, bundle.payer));
    assert.strictEqual(response.status, 201);
}