const { get } = require("../utility/axios_common");
const { debugLog } = require("../utility/helpers");

const api_config_host = process.env.api_config_host;

function apiConfigHealthCheck() {    
    const host = `${api_config_host}/info`;
    debugLog(`Calling endpoint: [${host}]`);
    return get(host, {
        headers: {
            "Host": process.env.host_header,
            "Ocp-Apim-Subscription-Key": process.env.API_CONFIG_SUBSCRIPTION_KEY
        }
    })
}

function readCreditorInstitution(organizationFiscalCode) {  
    const host = `${api_config_host}/creditorinstitutions/${organizationFiscalCode}`;
    debugLog(`Calling endpoint: [${host}]`);  
    return get(host, {
        headers: {
            "Ocp-Apim-Subscription-Key": process.env.API_CONFIG_SUBSCRIPTION_KEY
        }
    })
}

function readCreditorInstitutionIbans(organizationFiscalCode) {  
    const host = `${api_config_host}/creditorinstitutions/${organizationFiscalCode}/ibans`;
    debugLog(`Calling endpoint: [${host}]`);  
    return get(host, {
        headers: {
            "Ocp-Apim-Subscription-Key": process.env.API_CONFIG_SUBSCRIPTION_KEY
        }
    })
}

function readCreditorInstitutionBroker(brokerId) {      
    const host = `${api_config_host}/brokers/${brokerId}`;
    debugLog(`Calling endpoint: [${host}]`);  
    return get(host, {
        headers: {
            "Ocp-Apim-Subscription-Key": process.env.APICONFIG_SUBSCRIPTION_KEY
        }
    })
}

function readStation(stationId) {      
    const host = `${api_config_host}/stations/${stationId}`;
    debugLog(`Calling endpoint: [${host}]`);  
    return get(host, {
        headers: {
            "Ocp-Apim-Subscription-Key": process.env.APICONFIG_SUBSCRIPTION_KEY
        }
    })
}

function readCIStationAssociation(stationId, organizationFiscalCode) {   
    const host = `${api_config_host}/stations/${stationId}/creditorinstitutions/${organizationFiscalCode}`;
    debugLog(`Calling endpoint: [${host}]`);     
    return get(host, {
        headers: {
            "Ocp-Apim-Subscription-Key": process.env.APICONFIG_SUBSCRIPTION_KEY
        }
    })
}

function readPSP(pspId) {      
    const host = `${api_config_host}/paymentserviceproviders/${pspId}`;
    debugLog(`Calling endpoint: [${host}]`);  
    return get(host, {
        headers: {
            "Ocp-Apim-Subscription-Key": process.env.APICONFIG_SUBSCRIPTION_KEY
        }
    })
}

function readChannel(channelId) {      
    const host = `${api_config_host}/channels/${channelId}`;
    debugLog(`Calling endpoint: [${host}]`);  
    return get(host, {
        headers: {
            "Ocp-Apim-Subscription-Key": process.env.APICONFIG_SUBSCRIPTION_KEY
        }
    })
}

module.exports = {
    apiConfigHealthCheck,
    readChannel,
    readCreditorInstitution,
    readCreditorInstitutionBroker,
    readCreditorInstitutionIbans,
    readCIStationAssociation,
    readPSP,
    readStation
}