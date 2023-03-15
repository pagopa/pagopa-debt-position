const {
    createDebtPosition,
    updateDebtPosition,
    publishDebtPosition,
    deleteDebtPosition,
    getDebtPositionList,
    getDebtPosition,
    payPaymentOption,
    reportTransfer,
    createAndPublishDebtPosition,
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

async function executeDebtPositionUpdate(bundle, idOrg, iupd) {
    bundle.iupd = iupd;
    let response = await updateDebtPosition(idOrg, iupd, bundle);
    bundle.responseToCheck = response;
}

async function executeDebtPositionGetList(bundle, idOrg, dueDateFrom, dueDateTo, paymentDateFrom, paymentDateTo, status) {
    let response = await getDebtPositionList(idOrg, dueDateFrom, dueDateTo, paymentDateFrom, paymentDateTo, status);
    bundle.responseToCheck = response;
}

async function executeDebtPositionGet(bundle, idOrg, iupd) {
    let response = await getDebtPosition(idOrg, iupd);
    bundle.payer.companyName = response.data.companyName;
}

async function executeDebtPositionDeletion(bundle, idOrg, iupd) {
    let response = await deleteDebtPosition(idOrg, iupd);
    bundle.responseToCheck = response;
}

async function executeDebtPositionPublish(bundle, idOrg, iupd) {
    delete bundle.responseToCheck;
    let response = await publishDebtPosition(idOrg, iupd);
    bundle.responseToCheck = response;
}

async function executePaymentOptionPay(bundle, idOrg, iuv) {
    bundle.paymentDate = new Date();
    let response = await payPaymentOption(idOrg, iuv, bundle);
}

async function executeReportTransfer(bundle, idOrg) {
    let iuv = bundle.debtPosition.iuv1;
    let idTransfer = '1';
    let response = await reportTransfer(idOrg, iuv, idTransfer);
}

async function executeDebtPositionCreationAndPublication(bundle, idOrg, iupd) {
    bundle.organizationCode = idOrg;
    bundle.debtPosition = buildDebtPositionDynamicData(bundle, iupd);
    let response = await createAndPublishDebtPosition(bundle.organizationCode, buildCreateDebtPositionRequest(bundle.debtPosition, bundle.payer));
    bundle.responseToCheck = response;
}

module.exports = {
    executeDebtPositionCreation,
    executeDebtPositionDeletion,
    executeDebtPositionGetList,
    executeDebtPositionUpdate,
    executeDebtPositionGet,
    executeDebtPositionPublish,
    executePaymentOptionPay,
    executeReportTransfer,
    executeDebtPositionCreationAndPublication,
}