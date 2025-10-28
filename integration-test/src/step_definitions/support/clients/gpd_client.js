const { get, post, del, put, patch } = require("../utility/axios_common");
const { toLog  } = require("../utility/helpers");
const fs = require("fs");

const ENV = process.env.environment;
const GPD_HOST = process.env.gpd_host;
const GPD_HOST_V2 = process.env.gpd_host_v2;
const GPD_API_MASSIVE = (orgId) => process.env.gpd_api_massive.replace('{orgId}', orgId);
const API_TIMEOUT = process.env.api_timeout;

const GPD_EXTERNAL_HOST = process.env.gpd_external_host;

function  gpdHealthCheck() {
    return get(GPD_EXTERNAL_HOST + `/info`, {
        headers: {
            "Ocp-Apim-Subscription-Key": process.env.API_SUBSCRIPTION_KEY
        }
    })
}

function getVersionUri(version) {
    let version_uri = `/${version}`;
    if (ENV === 'local' && version === 'v1') {version_uri = '';} // in local v1 version is not present in the url
    return version_uri;
}

function createDebtPosition(orgId, body, segCodes, toPublish = false, version = "v1") {
	const params = {}
	if (segCodes) {params.segregationCodes = segCodes}
    if (toPublish) {params.toPublish = toPublish}
    toLog("[createDebtPosition] URL:" + GPD_EXTERNAL_HOST + getVersionUri(version) + `/organizations/${orgId}/debtpositions`)
    toLog("[createDebtPosition] BODY:" + JSON.stringify(body))
    return post(GPD_EXTERNAL_HOST + getVersionUri(version) + `/organizations/${orgId}/debtpositions`, body, {
        timeout: API_TIMEOUT,
        params,
        headers: {
            "Ocp-Apim-Subscription-Key": process.env.API_SUBSCRIPTION_KEY,
            "Content-Type": "application/json"
        }
    })
}

function createMassiveDebtPositions(orgId, body, segCodes) {
	const params = {}
	if (segCodes) {params.segregationCodes = segCodes}
    return post(GPD_HOST_V2 + GPD_API_MASSIVE(orgId), body, {
        timeout: API_TIMEOUT,
        params,
        headers: {
            "Ocp-Apim-Subscription-Key": process.env.API_SUBSCRIPTION_KEY,
            "Content-Type": "application/json"
        }
    })
}

function updateDebtPosition(orgId, iupd, body, segCodes, version = "v1") {
	const params = {}
	if (segCodes) {params.segregationCodes = segCodes}
    toLog("[updateDebtPosition] URL:" + GPD_EXTERNAL_HOST + getVersionUri(version) + `/organizations/${orgId}/debtpositions/${iupd}`)
    toLog("[updateDebtPosition] BODY:" + JSON.stringify(body))
    return put(GPD_EXTERNAL_HOST + getVersionUri(version) + `/organizations/${orgId}/debtpositions/${iupd}`, body, {
        timeout: API_TIMEOUT,
        params,
        headers: {
            "Ocp-Apim-Subscription-Key": process.env.API_SUBSCRIPTION_KEY,
            "Content-Type": "application/json"
        }
    })
}

function publishDebtPosition(orgId, iupd, segCodes, version = "v1") {
	const params = {}
	if (segCodes) {params.segregationCodes = segCodes}
    return post(GPD_EXTERNAL_HOST + getVersionUri(version) + `/organizations/${orgId}/debtpositions/${iupd}/publish`, "", {
        timeout: API_TIMEOUT,
        params,
        headers: {
            "Ocp-Apim-Subscription-Key": process.env.API_SUBSCRIPTION_KEY,
            "Content-Type": "application/json"
        }
    })
}

function invalidateDebtPosition(orgId, iupd, segCodes, version = "v1") {
	const params = {}
	if (segCodes) {params.segregationCodes = segCodes}
    return post(GPD_EXTERNAL_HOST + getVersionUri(version) + `/organizations/${orgId}/debtpositions/${iupd}/invalidate`, "", {
        timeout: API_TIMEOUT,
        params,
        headers: {
            "Ocp-Apim-Subscription-Key": process.env.API_SUBSCRIPTION_KEY,
            "Content-Type": "application/json"
        }
    })
}

function getDebtPositionList(orgId, dueDateFrom, dueDateTo, paymentDateFrom, paymentDateTo, status, segCodes, version = "v1") {
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

    return get(GPD_EXTERNAL_HOST + getVersionUri(version) + `/organizations/${orgId}/debtpositions`, {
        timeout: API_TIMEOUT,
        params,
        headers: {
            "Ocp-Apim-Subscription-Key": process.env.API_SUBSCRIPTION_KEY,
            "Content-Type": "application/json"
        }
    })
}

function getDebtPosition(orgId, iupd, segCodes, version = "v1") {
	const params = {}
	if (segCodes) {params.segregationCodes = segCodes}
    return get(GPD_EXTERNAL_HOST + getVersionUri(version) + `/organizations/${orgId}/debtpositions/${iupd}`, {
        timeout: API_TIMEOUT,
        headers: {
            "Ocp-Apim-Subscription-Key": process.env.API_SUBSCRIPTION_KEY,
            "Content-Type": "application/json"
        }
    })
}

function getDebtPositionByIUV(orgId, iuv, segCodes) {
    const params = {}
    if (segCodes) {params.segregationCodes = segCodes}
    toLog("[getDebtPositionByIUV] URL:" + GPD_HOST + `/organizations/${orgId}/paymentoptions/${iuv}/debtposition`)
    toLog("[getDebtPositionByIUV] BODY:" + JSON.stringify(segCodes))
    return get(GPD_HOST + `/organizations/${orgId}/paymentoptions/${iuv}/debtposition`, {
        timeout: API_TIMEOUT,
        headers: {
            "Ocp-Apim-Subscription-Key": process.env.API_SUBSCRIPTION_KEY,
            "Content-Type": "application/json"
        }
    })
}

function getPaymentOptionByIuv(orgId, iuv) {
    return get(GPD_HOST + `/organizations/${orgId}/paymentoptions/${iuv}`, {
        timeout: API_TIMEOUT,
        headers: {
            "Ocp-Apim-Subscription-Key": process.env.API_SUBSCRIPTION_KEY,
            "Content-Type": "application/json"
        }
    })
}

function deleteDebtPosition(orgId, iupd, segCodes, version = "v1") {
	const params = {}
	if (segCodes) {params.segregationCodes = segCodes}
    return del(GPD_EXTERNAL_HOST + getVersionUri(version) + `/organizations/${orgId}/debtpositions/${iupd}`, {
        timeout: API_TIMEOUT,
        params,
        headers: {
            "Ocp-Apim-Subscription-Key": process.env.API_SUBSCRIPTION_KEY,
            "Content-Type": "application/json"
        }
    })
}

function payPaymentOption(orgId, iuv, body) {
    toLog("[payPaymentOption] URL:" + GPD_HOST + `/organizations/${orgId}/paymentoptions/${iuv}/pay`)
    toLog("[payPaymentOption] BODY:" + JSON.stringify(body))
    return post(GPD_HOST + `/organizations/${orgId}/paymentoptions/${iuv}/pay`, body, {
        timeout: API_TIMEOUT,
        headers: {
            "Ocp-Apim-Subscription-Key": process.env.API_SUBSCRIPTION_KEY,
            "Content-Type": "application/json"
        }
    })
}

function reportTransfer(orgId, iuv, idTransfer) {
    return post(GPD_HOST + `/organizations/${orgId}/paymentoptions/${iuv}/transfers/${idTransfer}/report`, "", {
        timeout: API_TIMEOUT,
        headers: {
            "Ocp-Apim-Subscription-Key": process.env.API_SUBSCRIPTION_KEY,
            "Content-Type": "application/json"
        }
    })
}

function  updateNotificationFee(orgId, iuv, body) {
    toLog("[updateNotificationFee] URL:" + GPD_HOST + `/organizations/${orgId}/paymentoptions/${iuv}/notificationfee`)
    toLog("[updateNotificationFee] BODY: " + JSON.stringify(body))
    return put(GPD_HOST + `/organizations/${orgId}/paymentoptions/${iuv}/notificationfee`, body, {
        timeout: API_TIMEOUT,
        headers: {
            "Ocp-Apim-Subscription-Key": process.env.API_SUBSCRIPTION_KEY,
            "Content-Type": "application/json"
        }
    })
}

function createAndPublishDebtPosition(orgId, body, version = "v1") {
    return post(GPD_EXTERNAL_HOST + getVersionUri(version) + `/organizations/${orgId}/debtpositions`, body, {
        timeout: API_TIMEOUT,
        params: {
            toPublish: "True",
        },
        headers: {
            "Ocp-Apim-Subscription-Key": process.env.API_SUBSCRIPTION_KEY,
            "Content-Type": "application/json"
        }
    })
}

function updateAndPublishDebtPosition(orgId, iupd, body, version = "v1") {
    return put(GPD_EXTERNAL_HOST + getVersionUri(version) + `/organizations/${orgId}/debtpositions/${iupd}`, body, {
        timeout: API_TIMEOUT,
        params: {
            toPublish: "True",
        },
        headers: {
            "Ocp-Apim-Subscription-Key": process.env.API_SUBSCRIPTION_KEY,
            "Content-Type": "application/json"
        }
    })
}

function updateTransferIbanMassive(orgId, oldIban, newIban, version = "v1") {
    return patch(GPD_EXTERNAL_HOST + getVersionUri(version) + `/organizations/${orgId}/debtpositions/transfers?oldIban=${oldIban}&limit=10`, {newIban}, {
        timeout: API_TIMEOUT,
        headers: {
            "Ocp-Apim-Subscription-Key": process.env.API_SUBSCRIPTION_KEY,
            "Content-Type": "application/json"
        }
    })
}

async function verifyPaymentOptions(organizationFiscalCode, nav, config = {}, version = "v1") {
	const url = GPD_EXTERNAL_HOST + getVersionUri(version) + `/payment-options/organizations/${organizationFiscalCode}/notices/${nav}`;
	toLog("[verifyPaymentOptions] URL:" + url);
	return await post(url, {}, {
		timeout: API_TIMEOUT,
		headers: {
			"Ocp-Apim-Subscription-Key": process.env.API_SUBSCRIPTION_KEY,
			"Content-Type": "application/json"
		}
	});
}

module.exports = {
    gpdHealthCheck,
    createDebtPosition,
    publishDebtPosition,
    updateDebtPosition,
    updateNotificationFee,
    getDebtPositionList,
    getDebtPosition,
    getDebtPositionByIUV,
    deleteDebtPosition,
    payPaymentOption,
    reportTransfer,
    createAndPublishDebtPosition,
    updateAndPublishDebtPosition,
    getPaymentOptionByIuv,
    invalidateDebtPosition,
    createMassiveDebtPositions,
    updateTransferIbanMassive,
	verifyPaymentOptions
}