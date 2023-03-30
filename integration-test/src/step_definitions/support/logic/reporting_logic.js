const assert = require("assert");
const { QueueServiceClient } = require("@azure/storage-queue");
const { sendReportFlow, getReportFlows } = require("../clients/nodo_client");
const { getFlow, getFlowList } = require("../clients/reporting_client");
const { buildReportFlowForDebtPositionRequest, buildReportFlowCreationRequest, buildReportFlowsRetrieveRequest } = require("../utility/request_builders");
const { debugLog } = require("../utility/helpers");


async function forceReportingBatchStart(bundle) {
    console.log(` - When the reporting batch analyzes the reporting flows for the organization [${bundle.creditorInstitution.id}]..`);
    const queueServiceClient = QueueServiceClient.fromConnectionString(process.env.FLOW_SA_CONNECTION_STRING);
    const queueClient = queueServiceClient.getQueueClient(process.env.REPORTING_BATCH_QUEUE);
    const messageText = `{"idPA":["${bundle.creditorInstitution.id}"],"retry":0}`;
    const base64MessageText = Buffer.from(messageText).toString('base64');
    let response = queueClient.sendMessage(base64MessageText);
    debugLog(`Forcing reporting batch triggering execution with message in Base64: [${messageText}] returned the response: ${JSON.stringify(response)}`);    
}

async function retrieveReportFlow(bundle) {
    console.log(` - Then the client asks the detail for one of the report flows with id [${bundle.flow.id}] for date [${bundle.flow.dateAndTime}]..`);
    bundle.response = await getFlow(bundle.creditorInstitution.id, bundle.flow.id, bundle.flow.dateAndTime);
    debugLog(`Report flow retrieving API invocation returned HTTP status code: ${bundle.response.status} with body: ${JSON.stringify(bundle.response.data)}`);
}

async function retrieveReportFlowList(bundle) {
    console.log(` - Then the client asks the flow list for the organization with id [${bundle.creditorInstitution.id}]..`);
    bundle.response = await getFlowList(bundle.creditorInstitution.id);
    debugLog(`Report flow list retrieving API invocation returned HTTP status code: ${bundle.response.status} with body: ${JSON.stringify(bundle.response.data)}`);
}

async function sendReportFlowToNode(bundle) {
    console.log(" - Given a report flow sent to Nodo..");
    let reportFlowContent = Buffer.from(buildReportFlowForDebtPositionRequest(bundle)).toString('base64');
    let response = await sendReportFlow(buildReportFlowCreationRequest(bundle, reportFlowContent));
    debugLog(`Report flow sending to Nodo API invocation returned HTTP status code: ${response.status} with body: ${JSON.stringify(response.data)}`);
    assert.match(response.data, new RegExp(`<esito>OK</esito>`, "g"));    

    response = await getReportFlows(buildReportFlowsRetrieveRequest(bundle));
    debugLog(`Report flow retrieving to Nodo API invocation returned HTTP status code: ${response.status} with body: ${JSON.stringify(response.data)}`);
    assert.match(response.data, new RegExp(`<totRestituiti>.+</totRestituiti>`, "g")); 
}

async function waitWholeReportingProcessExecution() {
    console.log(` - -> the client waits its execution for [${process.env.reporting_batch_wait_execution_sec}] seconds. Please wait..`);
    await new Promise(resolve => setTimeout(resolve, process.env.reporting_batch_wait_execution_sec * 1000));
    console.log(` - -> ended waiting for [${process.env.reporting_batch_wait_execution_sec}] seconds..`);
}


module.exports = {
    forceReportingBatchStart,
    retrieveReportFlow,
    retrieveReportFlowList,
    sendReportFlowToNode,
    waitWholeReportingProcessExecution
}