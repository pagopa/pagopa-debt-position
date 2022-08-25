const {post, get, put, del} = require("./common");
const fs = require("fs");

let rawdata = fs.readFileSync('./config/properties.json');
let properties = JSON.parse(rawdata);
const payments_host = properties.payments_host;
const payments_info = properties.payments_info;
const sub_key = properties.sub_key;

function healthCheck() {
    return get(payments_info, {
        headers: {
            "Ocp-Apim-Subscription-Key": sub_key
        }
    })
}

function demandPaymentNotice(body) {
    return post(payments_host, body)
}

module.exports = {
    healthCheck,
    demandPaymentNotice
}
