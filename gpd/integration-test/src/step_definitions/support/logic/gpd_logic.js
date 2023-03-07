const {
    createDebtPosition,
    publishDebtPosition,
    deleteDebtPosition,
    getDebtPositionList,
} = require("../clients/gpd_client");

const {
    buildDebtPositionDynamicData,
    buildCreateDebtPositionRequest
} = require("../utility/request_builders");

async function executeDebtPositionCreation(bundle, idOrg, iupd) {
    bundle.organizationCode = idOrg;
    bundle.debtPosition = buildDebtPositionDynamicData(bundle, iupd);
    let response = await createDebtPosition(bundle.organizationCode, buildCreateDebtPositionRequest(bundle.debtPosition, bundle.payer));
    bundle.responseToCheck = response;
}

async function executeDebtPositionGetList(bundle, idOrg) {
    let response = await getDebtPositionList(idOrg);
    bundle.responseToCheck = response;
}

async function executeDebtPositionDeletion(idOrg, iupd) {
    let response = await deleteDebtPosition(idOrg, iupd);
}

module.exports = {
    executeDebtPositionCreation,
    executeDebtPositionDeletion,
    executeDebtPositionGetList,
}