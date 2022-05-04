import http from 'k6/http';
import { check } from 'k6';
import { generateFakeStringArray, randomString } from './modules/helpers.js';

export let options = {
	summaryTrendStats: ['avg', 'min', 'med', 'max', 'p(95)', 'p(99)', 'p(99.99)', 'count'],
	stages: [
		{ duration: '1m', target: 50 }, // simulate ramp-up of traffic from 1 to 50 users over 1 minutes.
	],
	thresholds: {
		http_req_failed: ['rate<0.01'], // http errors should be less than 1%
		http_req_duration: ['p(99)<1500'], // 99% of requests must complete below 1500ms
		'http_req_duration{method:GET}': ['p(99)<1500'] // 99% of requests must complete below 1500ms
	},
};




export function setup() {

	// setup code
	const creditor_institution_codes = generateFakeStringArray(11, `${__ENV.NUM_OF_ORGANIZATIONS}`, "0123456789");
	const flow_ids = generateFakeStringArray(32, `${__ENV.NUM_FLOW_ID_FOR_ORG}`, "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789");


	for (let y = 0; y < creditor_institution_codes.length; y++) {
		for (let i = 0; i < flow_ids.length; i++) {
			let flowTableURL = `${__ENV.FLOW_TABLE_URL}` + `${__ENV.ACCESS_SIGNATURE_QUERY_STRING}`;
			let flowId = flow_ids[i];
			let creditorInstitutionCode = creditor_institution_codes[y];
			let flowDate = new Date();
			let payload = JSON.stringify(
				{
					"flowDate": flowDate,
					"PartitionKey": creditorInstitutionCode,
					"RowKey": flowId
				}
			);

			let params = {
				headers: {
					'Content-Type': 'application/json',
					'Accept': 'application/json;odata=fullmetadata'
				},
			};

			let r = http.post(flowTableURL, payload, params);

			console.log("SETUP - flowTableURL: " + flowTableURL + ",PartitionKey: " + creditorInstitutionCode + ",RowKey: " + flowId + ",Status: " + r.status);

			check(r, {
				'status is 201': (r) => r.status === 201,
			});
		}
	}

	const data = {
		creditor_institution_codes: creditor_institution_codes,
		flow_ids: flow_ids
	};

	return { data: data };
}



export default function(data) {
	let creditor_institution_codes = data.data.creditor_institution_codes;
	getFlowList(creditor_institution_codes);
}

function getFlowList(creditor_institution_codes) {

	let urlBasePath = `${__ENV.BASE_URL}`

	// getFlowList with random creditor_institution_code retrieval from the array
	let randomCreditorInstitutionCode = creditor_institution_codes[Math.floor(Math.random() * creditor_institution_codes.length)];

	let url = `${urlBasePath}/organizations/${randomCreditorInstitutionCode}/reportings`;

	let r = http.get(url);

	console.log("FUNCTION - getFlowListURL " + url + "; Status " + r.status);

	check(r, {
		'status is 200': (r) => r.status === 200
	});
}

export function teardown(data) {
	let creditor_institution_codes = data.data.creditor_institution_codes;
	let flow_ids = data.data.flow_ids;
	// teardown code
	for (let y = 0; y < creditor_institution_codes.length; y++) {
		for (let i = 0; i < flow_ids.length; i++) {

			let flowId = flow_ids[i];
			let creditorInstitutionCode = creditor_institution_codes[y];

			let entityToDelete = `(PartitionKey='${creditorInstitutionCode}', RowKey='${flowId}')`;

			let flowTableURL = `${__ENV.FLOW_TABLE_URL}` + entityToDelete + `${__ENV.ACCESS_SIGNATURE_QUERY_STRING}`;

			let params = {
				headers: {
					'Accept': 'application/json;odata=fullmetadata',
					'If-Match': '*',
				},
			};

			let r = http.del(flowTableURL, null, params);

			console.log("TEARDOWN - flowTableURL " + flowTableURL + "; Status " + r.status);

			check(r, {
				'status is 204': (r) => r.status === 204,
			});

		}
	}
}