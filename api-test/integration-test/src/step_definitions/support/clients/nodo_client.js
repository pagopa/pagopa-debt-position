const { post } = require("../utility/axios_common");
const { debugLog } = require("../utility/helpers");

const nodo_host = process.env.nodo_host;

function activatePaymentNotice(body) {
    const host = `${nodo_host}/node-for-psp/v1`;
    debugLog(`Calling endpoint: [${host}] for [activatePaymentNotice] with body: [${JSON.stringify(body)}]`);
    return post(host, body, {
        timeout: 10000,
        headers: {
            'Content-Type': 'text/xml',
            'SOAPAction': 'activatePaymentNotice',
        }
    })
}

function getReportFlows(body) {   
    const host = `${nodo_host}/nodo-per-pa/v1`;
    debugLog(`Calling endpoint: [${host}] for [nodoChiediElencoFlussiRendicontazione] with body: [${JSON.stringify(body)}]`);
    return post(host, body, {
        timeout: 10000,
        headers: {
            "Content-Type": "text/xml",
            "SOAPAction": "nodoChiediElencoFlussiRendicontazione"
        }
    })
}

function sendPaymentOutcome(body) {
    const host = `${nodo_host}/node-for-psp/v1`;
    debugLog(`Calling endpoint: [${host}] for [sendPaymentOutcome] with body: [${JSON.stringify(body)}]`);
    return post(host, body, {
        timeout: 10000,
        headers: {
            'Content-Type': 'text/xml',
            'SOAPAction': 'sendPaymentOutcome',
        }
    })
}

function sendReportFlow(body) {   
    const host = `${nodo_host}/nodo-per-psp/v1`;
    debugLog(`Calling endpoint: [${host}] for [nodoInviaFlussoRendicontazione] with body: [${JSON.stringify(body)}]`);
    return post(host, body, {
        timeout: 10000,
        headers: {
            "Content-Type": "text/xml",
            "SOAPAction": "nodoInviaFlussoRendicontazione"
        }
    })
}

module.exports = {
    activatePaymentNotice,
    getReportFlows,
    sendPaymentOutcome,
    sendReportFlow
}