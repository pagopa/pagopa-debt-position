const {get, post} = require("../utility/axios_common");
const { debugLog } = require("../utility/helpers");

const gpd_host = process.env.gpd_host;

function gpdHealthCheck() {
    const host = `${gpd_host}/info`;
    debugLog(`Calling endpoint: [${host}]`);
    return get(host, {
        headers: {
            "Host": process.env.host_header,
            "Ocp-Apim-Subscription-Key": process.env.GPD_SUBSCRIPTION_KEY
        }
    })
}

function createDebtPosition(organizationFiscalCode, body) {  
    const host = `${gpd_host}/organizations/${organizationFiscalCode}/debtpositions`;
    debugLog(`Calling endpoint: [${host}] with body: [${JSON.stringify(body)}]`); 
    return post(host, body, {
        timeout: 10000,
        headers: {
            "Ocp-Apim-Subscription-Key": process.env.GPD_SUBSCRIPTION_KEY,
            "Content-Type": "application/json"
        }
    })
}

function getPaymentOptionDetail(organizationFiscalCode, iuv) {
    const host = `${gpd_host}/organizations/${organizationFiscalCode}/debtpositions/${iuv}`;
    debugLog(`Calling endpoint: [${host}]`);
    return get(host, {
        timeout: 10000,
        headers: {
            "Ocp-Apim-Subscription-Key": process.env.GPD_SUBSCRIPTION_KEY,
            "Content-Type": "application/json"
        }
    });
}

function payDebtPosition(organizationFiscalCode, iupd, body) {
    const host = `${gpd_host}/organizations/${organizationFiscalCode}/paymentoptions/${iupd}/pay`;
    debugLog(`Calling endpoint: [${host}] with body: [${JSON.stringify(body)}]`);
    return post(host, body, {
        timeout: 10000,
        headers: {
            "Ocp-Apim-Subscription-Key": process.env.GPD_SUBSCRIPTION_KEY,
            "Content-Type": "application/json"
        }
    })
}

function publishDebtPosition(organizationFiscalCode, iupd) {
    const host = `${gpd_host}/organizations/${organizationFiscalCode}/debtpositions/${iupd}/publish`;
    debugLog(`Calling endpoint: [${host}]`);
    return post(host, "", {
        timeout: 10000,
        headers: {
            "Ocp-Apim-Subscription-Key": process.env.GPD_SUBSCRIPTION_KEY,
            "Content-Type": "application/json"
        }
    })
}


module.exports = {
    createDebtPosition,
    getPaymentOptionDetail,
    gpdHealthCheck,
    payDebtPosition,
    publishDebtPosition
}