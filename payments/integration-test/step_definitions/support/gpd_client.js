const {get} = require("./common");
const fs = require("fs");

let rawdata = fs.readFileSync('./config/properties.json');
let properties = JSON.parse(rawdata);
const gpd_host = properties.gpd_host;

function gpdHealthCheck() {
    return get(gpd_host + `/info`)
}

module.exports = {
    gpdHealthCheck
}