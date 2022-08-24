const {post, get, put, del} = require("./common");
const fs = require("fs");

let rawdata = fs.readFileSync('./config/properties.json');
let properties = JSON.parse(rawdata);
const payments_host = properties.payments_host;

function healthCheck() {
    return get(payments_host + `/info`)
}

function demandPaymentNotice(body) {
    return post(payments_host, body)
}

module.exports = {
    healthCheck,
    demandPaymentNotice
}
