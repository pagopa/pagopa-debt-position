const {post, get, put, del} = require("../utility/axios_common");
const ip = require('ip');
const { debugLog } = require("../utility/helpers");

const payments_soap_host = process.env.payments_soap_host;
const payments_rest_host = process.env.payments_rest_host;
const ipAddress = ip.address();

function paymentsHealthCheck() {
    const host = `${payments_rest_host}/info`;
    debugLog(`Calling endpoint: [${host}]`);
    return get(host, {
        headers: {
            "Host": process.env.host_header,
            "X-Forwarded-For": ipAddress,
            "Ocp-Apim-Subscription-Key": process.env.PAYMENTS_REST_SUBSCRIPTION_KEY
        }
    })
}

function demandPaymentNotice(body) {
    const host = `${payments_soap_host}`;
    debugLog(`Calling endpoint: [${host}] for [paDemandPaymentNotice] with body: [${JSON.stringify(body)}]`);
    return post(host, body, {
        timeout: 10000,
        headers: {
            'Content-Type': 'text/xml',
            'SOAPAction': 'paDemandPaymentNotice',
            "Ocp-Apim-Subscription-Key": process.env.PAYMENTS_SOAP_SUBSCRIPTION_KEY
        }
    })
}

function verifyPaymentNotice(body) {
    const host = `${payments_soap_host}`;
    debugLog(`Calling endpoint: [${host}] for [paVerifyPaymentNotice] with body: [${JSON.stringify(body)}]`);
    return post(host, body, {
        timeout: 10000,
        headers: {
            'Content-Type': 'text/xml',
            'SOAPAction': 'paVerifyPaymentNotice',
            "Ocp-Apim-Subscription-Key": process.env.PAYMENTS_SOAP_SUBSCRIPTION_KEY
        }
    })
}

function getPayment(body) {
    const host = `${payments_soap_host}`;
    debugLog(`Calling endpoint: [${host}] for [paGetPayment] with body: [${JSON.stringify(body)}]`);
    return post(host, body, {
        timeout: 10000,
        headers: {
            'Content-Type': 'text/xml',
            'SOAPAction': 'paGetPayment',
            "Ocp-Apim-Subscription-Key": process.env.PAYMENTS_SOAP_SUBSCRIPTION_KEY
        }
    })
}

function sendRT(body) {
    const host = `${payments_soap_host}`;
    debugLog(`Calling endpoint: [${host}] for [paSendRT] with body: [${JSON.stringify(body)}]`);
    return post(host, body, {
        timeout: 10000,
        headers: {
            'Content-Type': 'text/xml',
            'SOAPAction': 'paSendRT',
            "Ocp-Apim-Subscription-Key": process.env.PAYMENTS_SOAP_SUBSCRIPTION_KEY
        }
    })
}



module.exports = {
    demandPaymentNotice,
    getPayment,
    paymentsHealthCheck,
    sendRT,
    verifyPaymentNotice,
}