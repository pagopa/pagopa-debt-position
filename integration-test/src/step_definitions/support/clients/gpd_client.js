const { get, post, del, put } = require("../utility/axios_common");
const fs = require("fs");

const GPD_HOST = process.env.gpd_host;
const GPD_HOST_V2 = process.env.gpd_host_v2;
const GPD_API_MASSIVE = (orgId) => process.env.gpd_api_massive.replace('${orgId}', orgId);

const GPD_EXTERNAL_HOST = process.env.gpd_external_host;

function gpdHealthCheck() {
    return get(GPD_HOST + `/info`, {
        headers: {
            "Ocp-Apim-Subscription-Key": process.env.API_SUBSCRIPTION_KEY
        }
    })
}

function createDebtPosition(orgId, body, segCodes){
	const params = {}
	if (segCodes) {params.segregationCodes = segCodes}
    return post(GPD_HOST + `/organizations/${orgId}/debtpositions`, body, {
        timeout: 20000,
        params,
        headers: {
            "Ocp-Apim-Subscription-Key": process.env.API_SUBSCRIPTION_KEY,
            "Content-Type": "application/json"
        }
    })
}

function createMassiveDebtPositions(orgId, body, segCodes){
	const params = {}
	if (segCodes) {params.segregationCodes = segCodes}
	let path = GPD_API_MASSIVE(orgId);
	console.log ("******************* " + GPD_HOST_V2 + path, orgId);
    return post(GPD_HOST_V2 + path, body, {
        timeout: 20000,
        params,
        headers: {
            "Ocp-Apim-Subscription-Key": process.env.API_SUBSCRIPTION_KEY,
            "Content-Type": "application/json"
        }
    })
}

function updateDebtPosition(orgId, iupd, body, segCodes) {
	const params = {}
	if (segCodes) {params.segregationCodes = segCodes}
    return put(GPD_HOST + `/organizations/${orgId}/debtpositions/${iupd}`, body, {
        timeout: 20000,
        params,
        headers: {
            "Ocp-Apim-Subscription-Key": process.env.API_SUBSCRIPTION_KEY,
            "Content-Type": "application/json"
        }
    })
}

function publishDebtPosition(orgId, iupd, segCodes) {
	const params = {}
	if (segCodes) {params.segregationCodes = segCodes}
    return post(GPD_HOST + `/organizations/${orgId}/debtpositions/${iupd}/publish`, "", {
        timeout: 20000,
        params,
        headers: {
            "Ocp-Apim-Subscription-Key": process.env.API_SUBSCRIPTION_KEY,
            "Content-Type": "application/json"
        }
    })
}

function invalidateDebtPosition(orgId, iupd, segCodes) {
	const params = {}
	if (segCodes) {params.segregationCodes = segCodes}
    return post(GPD_HOST + `/organizations/${orgId}/debtpositions/${iupd}/invalidate`, "", {
        timeout: 20000,
        params,
        headers: {
            "Ocp-Apim-Subscription-Key": process.env.API_SUBSCRIPTION_KEY,
            "Content-Type": "application/json"
        }
    })
}

function getDebtPositionList(orgId, dueDateFrom, dueDateTo, paymentDateFrom, paymentDateTo, status, segCodes) {
    const params = {}
    if (dueDateFrom != null) params.due_date_from = dueDateFrom;
    if (dueDateTo != null) params.due_date_to = dueDateTo;
    if (paymentDateFrom != null) params.payment_date_from = paymentDateFrom;
    if (paymentDateTo != null) params.payment_date_to = paymentDateTo;
    if (status != null) params.status = status;
    if (segCodes) {params.segregationCodes = segCodes}
    params.orderby = "INSERTED_DATE";
    params.ordering = "ASC";
    params.page = 0;

    return get(GPD_EXTERNAL_HOST + `/organizations/${orgId}/debtpositions`, {
        timeout: 20000,
        params,
        headers: {
            "Ocp-Apim-Subscription-Key": process.env.API_SUBSCRIPTION_KEY,
            "Content-Type": "application/json"
        }
    })
}

function getDebtPosition(orgId, iupd, segCodes) {
	const params = {}
	if (segCodes) {params.segregationCodes = segCodes}
    return get(GPD_HOST + `/organizations/${orgId}/debtpositions/${iupd}`, {
        timeout: 20000,
        headers: {
            "Ocp-Apim-Subscription-Key": process.env.API_SUBSCRIPTION_KEY,
            "Content-Type": "application/json"
        }
    })
}

function getPaymentOptionByIuv(orgId, iuv) {
    return get(GPD_HOST + `/organizations/${orgId}/paymentoptions/${iuv}`, {
        timeout: 20000,
        headers: {
            "Ocp-Apim-Subscription-Key": process.env.API_SUBSCRIPTION_KEY,
            "Content-Type": "application/json"
        }
    })
}

function deleteDebtPosition(orgId, iupd, segCodes) {
	const params = {}
	if (segCodes) {params.segregationCodes = segCodes}
    return del(GPD_HOST + `/organizations/${orgId}/debtpositions/${iupd}`, {
        timeout: 20000,
        params,
        headers: {
            "Ocp-Apim-Subscription-Key": process.env.API_SUBSCRIPTION_KEY,
            "Content-Type": "application/json"
        }
    })
}

function payPaymentOption(orgId, iuv, body) {
    return post(GPD_HOST + `/organizations/${orgId}/paymentoptions/${iuv}/pay`, body, {
        timeout: 20000,
        headers: {
            "Ocp-Apim-Subscription-Key": process.env.API_SUBSCRIPTION_KEY,
            "Content-Type": "application/json"
        }
    })
}

function reportTransfer(orgId, iuv, idTransfer) {
    return post(GPD_HOST + `/organizations/${orgId}/paymentoptions/${iuv}/transfers/${idTransfer}/report`, "", {
        timeout: 20000,
        headers: {
            "Ocp-Apim-Subscription-Key": process.env.API_SUBSCRIPTION_KEY,
            "Content-Type": "application/json"
        }
    })
}

function updateNotificationFee(orgId, iuv, body) {
    return put(GPD_HOST + `/organizations/${orgId}/paymentoptions/${iuv}/notificationfee`, body, {
        timeout: 20000,
        headers: {
            "Ocp-Apim-Subscription-Key": process.env.API_SUBSCRIPTION_KEY,
            "Content-Type": "application/json"
        }
    })
}

function createAndPublishDebtPosition(orgId, body) {
    return post(GPD_HOST + `/organizations/${orgId}/debtpositions`, body, {
        timeout: 20000,
        params: {
            toPublish: "True",
        },
        headers: {
            "Ocp-Apim-Subscription-Key": process.env.API_SUBSCRIPTION_KEY,
            "Content-Type": "application/json"
        }
    })
}

function updateAndPublishDebtPosition(orgId, iupd, body) {
    return put(GPD_HOST + `/organizations/${orgId}/debtpositions/${iupd}`, body, {
        timeout: 20000,
        params: {
            toPublish: "True",
        },
        headers: {
            "Ocp-Apim-Subscription-Key": process.env.API_SUBSCRIPTION_KEY,
            "Content-Type": "application/json"
        }
    })
}

module.exports = {
    gpdHealthCheck,
    createDebtPosition,
    publishDebtPosition,
    updateDebtPosition,
    updateNotificationFee,
    getDebtPositionList,
    getDebtPosition,
    deleteDebtPosition,
    payPaymentOption,
    reportTransfer,
    createAndPublishDebtPosition,
    updateAndPublishDebtPosition,
    getPaymentOptionByIuv,
    invalidateDebtPosition,
    createMassiveDebtPositions
}