const {post, get, put, del} = require("./common");
const fs = require("fs");

let rawdata = fs.readFileSync('./config/properties.json');
let properties = JSON.parse(rawdata);
const payments_host = properties.payments_host;
const payments_info = properties.payments_info;

function healthCheck() {
    return get(payments_info, {
        headers: {
            "Ocp-Apim-Subscription-Key": process.env.PAYMENTS_SUBSCRIPTION_KEY
        }
    })
}

function demandPaymentNotice(body) {
    return post(payments_host, body, {
        headers: {
            'Content-Type': 'text/xml',
            'SOAPAction': 'paDemandPaymentNotice'
        }
    })
}

module.exports = {
    healthCheck,
    demandPaymentNotice
}
