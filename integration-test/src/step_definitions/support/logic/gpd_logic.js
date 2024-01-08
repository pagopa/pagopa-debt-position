const {
    createDebtPosition,
    updateDebtPosition,
    updateNotificationFee,
    publishDebtPosition,
    deleteDebtPosition,
    getDebtPositionList,
    getDebtPosition,
    payPaymentOption,
    reportTransfer,
    createAndPublishDebtPosition,
    updateAndPublishDebtPosition,
    getPaymentOptionByIuv,
    invalidateDebtPosition,
    createMassiveDebtPositions
} = require("../clients/gpd_client");

const {
    buildDebtPositionDynamicData,
    buildCreateDebtPositionRequest,
    buildUpdateDebtPositionRequest,
    buildUpdateDebtPositionInfoRequest,
    buildCreateOK_KODebtPositionRequest,
    buildCreateMassiveDebtPositionRequest
} = require("../utility/request_builders");

async function executeDebtPositionCreation(bundle, idOrg, iupd) {
    bundle.organizationCode = idOrg;
    bundle.debtPosition = buildDebtPositionDynamicData(bundle, iupd);
    let response = await createDebtPosition(bundle.organizationCode, buildCreateDebtPositionRequest(bundle.debtPosition, bundle.payer));
    bundle.responseToCheck = response;
    bundle.createdDebtPosition = bundle.responseToCheck.data;
}

async function executeMassiveDebtPositionsCreation(bundle, idOrg, iupd) {
    bundle.organizationCode = idOrg;
    bundle.debtPosition = buildDebtPositionDynamicData(bundle, iupd);
    let response = await createMassiveDebtPositions(bundle.organizationCode, buildCreateMassiveDebtPositionRequest(bundle.debtPosition, bundle.payer));
    console.log (response);
    bundle.responseToCheck = response;
}

async function executeDebtPositionCreationWithSegregationCodes(bundle, idOrg, iupd) {
    bundle.organizationCode = idOrg;
    bundle.debtPosition = buildDebtPositionDynamicData(bundle, iupd);
    let segCodes = bundle.debtPosition.iuv1.slice(0, 2)
    let response = await createDebtPosition(bundle.organizationCode, buildCreateDebtPositionRequest(bundle.debtPosition, bundle.payer), segCodes);
    bundle.responseToCheck = response;
    bundle.createdDebtPosition = bundle.responseToCheck.data;
}

async function executeMassiveDebtPositionCreationWithSegregationCodes (bundle, idOrg, iupd) {
	bundle.organizationCode = idOrg;
    bundle.debtPosition = buildDebtPositionDynamicData(bundle, iupd);
    let segCodes = bundle.debtPosition.iuv1.slice(0, 2)
    let response = await createMassiveDebtPositions(bundle.organizationCode, buildCreateMassiveDebtPositionRequest(bundle.debtPosition, bundle.payer), segCodes);
    bundle.responseToCheck = response;
}

async function executeOKDebtPositionCreation(bundle, idOrg, iupd) {
    bundle.organizationCode = idOrg;
    bundle.debtPosition = buildDebtPositionDynamicData(bundle, iupd);
    let response = await createDebtPosition(bundle.organizationCode, buildCreateOK_KODebtPositionRequest(bundle, "OK"));
    bundle.responseToCheck = response;
    bundle.createdDebtPosition = bundle.responseToCheck.data;
}

async function executeKODebtPositionCreation(bundle, idOrg, iupd) {
    bundle.organizationCode = idOrg;
    bundle.debtPosition = buildDebtPositionDynamicData(bundle, iupd);
    let response = await createDebtPosition(bundle.organizationCode, buildCreateOK_KODebtPositionRequest(bundle, "KO"));
    bundle.responseToCheck = response;
    bundle.createdDebtPosition = bundle.responseToCheck.data;
}

async function executeDebtPositionUpdate(bundle, payer, idOrg, iupd) {
	let updatedDebtPosition=buildUpdateDebtPositionInfoRequest(bundle.createdDebtPosition, payer)
    updateDebtPosition.iupd = iupd;
    let response = await updateDebtPosition(idOrg, iupd, updatedDebtPosition);
    bundle.responseToCheck = response;
    bundle.updatedDebtPosition = bundle.responseToCheck.data;
}

async function executeDebtPositionUpdateWithSegregationCodes(bundle, payer, idOrg, iupd) {
    let updatedDebtPosition=buildUpdateDebtPositionInfoRequest(bundle.createdDebtPosition, payer)
    updateDebtPosition.iupd = iupd;
    let segCodes = updatedDebtPosition.paymentOption[0].iuv.slice(0, 2)
    let response = await updateDebtPosition(idOrg, iupd, updatedDebtPosition, segCodes);
    bundle.responseToCheck = response;
    bundle.updatedDebtPosition = bundle.responseToCheck.data;
}

async function executeDebtPositionNotificationFeeUpdate(bundle, idOrg, fee) {
    let iuv = bundle.debtPosition.iuv1;
    let response = await updateNotificationFee(idOrg, iuv, {notificationFee: fee});
    bundle.responseToCheck = response;    
}

async function executeDebtPositionNotificationFeeUpdateNodeOK(bundle, idOrg, fee) {
    let iuv = bundle.debtPosition.iuvOK;
    let response = await updateNotificationFee(idOrg, iuv, {notificationFee: fee});
    bundle.responseToCheck = response;    
}

async function executeDebtPositionNotificationFeeUpdateNodeKO(bundle, idOrg, fee) {
    let iuv = bundle.debtPosition.iuvKO;
    let response = await updateNotificationFee(idOrg, iuv, {notificationFee: fee});
    bundle.responseToCheck = response;    
}

async function executeDebtPositionGetList(bundle, idOrg, dueDateFrom, dueDateTo, paymentDateFrom, paymentDateTo, status) {
    let response = await getDebtPositionList(idOrg, dueDateFrom, dueDateTo, paymentDateFrom, paymentDateTo, status);
    bundle.responseToCheck = response;
}

async function executeDebtPositionGetListWithSegregationCodes(bundle, idOrg){
	let segCodes = bundle.debtPosition.iuv1.slice(0, 2)
	let response = await getDebtPositionList(idOrg, null, null, null, null, null, segCodes);
	bundle.responseToCheck = response;
}

async function executeDebtPositionGet(bundle, idOrg, iupd) {
    let response = await getDebtPosition(idOrg, iupd);
    bundle.payer.companyName = response.data.companyName;
    bundle.responseToCheck = response;
}

async function executeDebtPositionGetWithSegregationCodes(bundle, idOrg, iupd) {
	let segCodes = bundle.debtPosition.iuv1.slice(0, 2)
    let response = await getDebtPosition(idOrg, iupd, segCodes);
    bundle.payer.companyName = response.data.companyName;
    bundle.responseToCheck = response;
}

async function executePaymentOptionGetByIuv(bundle, idOrg, iuv) {
    let response = await getPaymentOptionByIuv(idOrg, iuv);
    bundle.responseToCheck = response;
}

async function executeDebtPositionDeletion(bundle, idOrg, iupd) {
    let response = await deleteDebtPosition(idOrg, iupd);
    bundle.responseToCheck = response;
}

async function executeDebtPositionDeletionWithSegregationCodes(bundle, idOrg, iupd) {
	let segCodes = bundle.debtPosition.iuv1.slice(0, 2)
    let response = await deleteDebtPosition(idOrg, iupd, segCodes);
    bundle.responseToCheck = response;
}

async function executeDebtPositionPublish(bundle, idOrg, iupd) {
    delete bundle.responseToCheck;
    let response = await publishDebtPosition(idOrg, iupd);
    bundle.responseToCheck = response;
}

async function executeDebtPositionPublishWithSegregationCodes(bundle, idOrg, iupd) {
    delete bundle.responseToCheck;
    let segCodes = bundle.debtPosition.iuv1.slice(0, 2)
    let response = await publishDebtPosition(idOrg, iupd, segCodes);
    bundle.responseToCheck = response;
}

async function executeDebtPositionInvalidateWithSegregationCodes(bundle, idOrg, iupd) {
    delete bundle.responseToCheck;
    let segCodes = bundle.debtPosition.iuv1.slice(0, 2)
    let response = await invalidateDebtPosition(idOrg, iupd, segCodes);
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
    let response = await createAndPublishDebtPosition(bundle.organizationCode, buildUpdateDebtPositionRequest(bundle.debtPosition, bundle.payer));
    bundle.responseToCheck = response;
}

async function executeDebtPositionUpdateAndPublication(bundle, idOrg, iupd) {
    bundle.organizationCode = idOrg;
    let response = await updateAndPublishDebtPosition(bundle.organizationCode, iupd, buildCreateDebtPositionRequest(bundle.debtPosition, bundle.payer));
    bundle.responseToCheck = response;
}

module.exports = {
    executeDebtPositionCreation,
    executeDebtPositionDeletion,
    executeDebtPositionGetList,
    executeDebtPositionUpdate,
    executeDebtPositionNotificationFeeUpdate,
    executeDebtPositionGet,
    executeDebtPositionPublish,
    executePaymentOptionPay,
    executeReportTransfer,
    executeDebtPositionCreationAndPublication,
    executeDebtPositionUpdateAndPublication,
    executePaymentOptionGetByIuv,
    executeKODebtPositionCreation,
    executeOKDebtPositionCreation,
    executeDebtPositionNotificationFeeUpdateNodeOK,
    executeDebtPositionNotificationFeeUpdateNodeKO,
    executeDebtPositionCreationWithSegregationCodes,
    executeDebtPositionUpdateWithSegregationCodes,
    executeDebtPositionGetWithSegregationCodes,
    executeDebtPositionGetListWithSegregationCodes,
    executeDebtPositionDeletionWithSegregationCodes,
    executeDebtPositionPublishWithSegregationCodes,
    executeDebtPositionInvalidateWithSegregationCodes,
    executeMassiveDebtPositionsCreation,
    executeMassiveDebtPositionCreationWithSegregationCodes
}