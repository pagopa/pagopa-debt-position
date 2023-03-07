const {
    createDebtPosition,
    publishDebtPosition,
    deleteDebtPosition
} = require("../clients/gpd_client");

const {
    buildDebtPositionDynamicData,
    buildCreateDebtPositionRequest
} = require("../utility/request_builders");

const assert = require("assert");

async function executeDebtPositionCreation(bundle, idOrg, iupd) {
    bundle.organizationCode = idOrg;
    bundle.debtPosition = buildDebtPositionDynamicData(bundle, iupd);
    let response = await createDebtPosition(bundle.organizationCode, buildCreateDebtPositionRequest(bundle.debtPosition, bundle.payer));
    bundle.responseToCheck = response;
}

async function executeDebtPositionDeletion(idOrg, iupd) {
    let response = await deleteDebtPosition(idOrg, iupd);
}

module.exports = {
    executeDebtPositionCreation,
    executeDebtPositionDeletion,
}