const {get} = require("../utility/axios_common");
const { debugLog } = require("../utility/helpers");

const reporting_analysis_host = process.env.reporting_analysis_host;

async function getFlow(organizationFiscalCode, flowId, date) {
    const host = `${reporting_analysis_host}/organizations/${organizationFiscalCode}/reportings/${flowId}/date/${date}`;
    debugLog(`Calling endpoint: [${host}]`);
    return get(host, {
        headers: {
            "Ocp-Apim-Subscription-Key": process.env.REPORTING_SUBSCRIPTION_KEY
        }
    });
}

async function getFlowList(organizationFiscalCode) {
    const host = `${reporting_analysis_host}/organizations/${organizationFiscalCode}/reportings`;
    debugLog(`Calling endpoint: [${host}]`);
    return get(host, {
        headers: {
            "Ocp-Apim-Subscription-Key": process.env.REPORTING_SUBSCRIPTION_KEY
        }
    });
}

module.exports = {
    getFlow,
    getFlowList
}