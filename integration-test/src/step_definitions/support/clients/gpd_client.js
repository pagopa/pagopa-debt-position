const { get, post, del, put } = require("../utility/axios_common");
const fs = require("fs");

const gpd_host = process.env.gpd_host;

function gpdHealthCheck() {
    return get(gpd_host + `/info`, {
        headers: {
            "Ocp-Apim-Subscription-Key": process.env.API_SUBSCRIPTION_KEY
        }
    })
}

function createDebtPosition(orgId, body, segCodes){
	const params = {}
	if (segCodes) {params.segregationCodes = segCodes}
    return post(gpd_host + `/organizations/${orgId}/debtpositions`, body, {
        timeout: 10000,
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
    return put(gpd_host + `/organizations/${orgId}/debtpositions/${iupd}`, body, {
        timeout: 10000,
        params,
        headers: {
            "Ocp-Apim-Subscription-Key": process.env.API_SUBSCRIPTION_KEY,
            "Content-Type": "application/json"
        }
    })
}

function publishDebtPosition(orgId, iupd) {
    return post(gpd_host + `/organizations/${orgId}/debtpositions/${iupd}/publish`, "", {
        timeout: 10000,
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

    return get(gpd_host + `/organizations/${orgId}/debtpositions/`, {
        timeout: 10000,
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
    return get(gpd_host + `/organizations/${orgId}/debtpositions/${iupd}`, {
        timeout: 10000,
        headers: {
            "Ocp-Apim-Subscription-Key": process.env.API_SUBSCRIPTION_KEY,
            "Content-Type": "application/json"
        }
    })
}

function getPaymentOptionByIuv(orgId, iuv) {
    return get(gpd_host + `/organizations/${orgId}/paymentoptions/${iuv}`, {
        timeout: 10000,
        headers: {
            "Ocp-Apim-Subscription-Key": process.env.API_SUBSCRIPTION_KEY,
            "Content-Type": "application/json"
        }
    })
}

function deleteDebtPosition(orgId, iupd, segCodes) {
	const params = {}
	if (segCodes) {params.segregationCodes = segCodes}
    return del(gpd_host + `/organizations/${orgId}/debtpositions/${iupd}`, {
        timeout: 10000,
        params,
        headers: {
            "Ocp-Apim-Subscription-Key": process.env.API_SUBSCRIPTION_KEY,
            "Content-Type": "application/json"
        }
    })
}

function payPaymentOption(orgId, iuv, body) {
    return post(gpd_host + `/organizations/${orgId}/paymentoptions/${iuv}/pay`, body, {
        timeout: 10000,
        headers: {
            "Ocp-Apim-Subscription-Key": process.env.API_SUBSCRIPTION_KEY,
            "Content-Type": "application/json"
        }
    })
}

function reportTransfer(orgId, iuv, idTransfer) {
    return post(gpd_host + `/organizations/${orgId}/paymentoptions/${iuv}/transfers/${idTransfer}/report`, "", {
        timeout: 10000,
        headers: {
            "Ocp-Apim-Subscription-Key": process.env.API_SUBSCRIPTION_KEY,
            "Content-Type": "application/json"
        }
    })
}

function updateNotificationFee(orgId, iuv, body) {
    return put(gpd_host + `/organizations/${orgId}/paymentoptions/${iuv}/notificationfee`, body, {
        timeout: 10000,
        headers: {
            "Ocp-Apim-Subscription-Key": process.env.API_SUBSCRIPTION_KEY,
            "Content-Type": "application/json"
        }
    })
}

function createAndPublishDebtPosition(orgId, body) {
    return post(gpd_host + `/organizations/${orgId}/debtpositions`, body, {
        timeout: 10000,
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
    return put(gpd_host + `/organizations/${orgId}/debtpositions/${iupd}`, body, {
        timeout: 10000,
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
    getPaymentOptionByIuv
}