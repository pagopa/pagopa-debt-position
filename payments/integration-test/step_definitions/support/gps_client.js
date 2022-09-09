const {post, get, put, del} = require("./common");
const fs = require("fs");

let rawdata = fs.readFileSync('./config/properties.json');
let properties = JSON.parse(rawdata);
const gps_host = properties.gps_host;

function gpsHealthCheck() {
    return get(gps_host + `/info`)
}

function createOrganization(idOrg, body) {
    return post(gps_host + `/organizations/${idOrg}`, body)
}

function deleteOrganization(idOrg) {
    return del(gps_host + `/organizations/${idOrg}`)
}

function createService(body) {
    return post(gps_host + `/services`, body)
}

function deleteService(serviceId) {
    return del(gps_host + `/services/${serviceId}`)
}


module.exports = {
    gpsHealthCheck,
    createOrganization,
    deleteOrganization,
    createService,
    deleteService
}
