const {get} = require("./common");
const fs = require("fs");

let rawdata = fs.readFileSync('./config/properties.json');
let properties = JSON.parse(rawdata);
const iuv_generator_host = properties.iuv_generator_host;

function iuvGenHealthCheck() {
    return get(iuv_generator_host + `/info`)
}

module.exports = {
    iuvGenHealthCheck
}