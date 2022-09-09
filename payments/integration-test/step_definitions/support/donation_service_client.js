const {get} = require("./common");
const fs = require("fs");

let rawdata = fs.readFileSync('./config/properties.json');
let properties = JSON.parse(rawdata);
const donation_service_host = properties.donation_host;

function donationHealthCheck() {
    return get(donation_service_host + `/info`)
}

module.exports = {
    donationHealthCheck
}