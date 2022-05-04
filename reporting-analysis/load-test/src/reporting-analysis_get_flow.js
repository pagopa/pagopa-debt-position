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

const file_to_load = open(`${__ENV.FILENAME}`);


export function setup() {

	// setup code
	const creditor_institution_codes = generateFakeStringArray(11, `${__ENV.NUM_OF_ORGANIZATIONS}`, "0123456789");
	const flow_ids = generateFakeStringArray(32, `${__ENV.NUM_FLOW_ID_FOR_ORG}`, "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789");
	const uploaded_files = [];
	const flow_dates = [];

	for (let y = 0; y < creditor_institution_codes.length; y++) {
		for (let i = 0; i < flow_ids.length; i++) {
			let flowId = flow_ids[i];
			let creditorInstitutionCode = creditor_institution_codes[y];
			let flowDate = new Date();
			let flowDateStr =  flowDate.getFullYear()+""+(flowDate.getMonth()+1).toString().padStart(2, '0')+""+flowDate.getDate().toString().padStart(2, '0');
			let fileName = flowDateStr + "%23%23" + creditorInstitutionCode + "%23%23" + flowId + ".xml"
			uploaded_files.push(fileName);
			flow_dates.push(flowDateStr);
			let flowBlobURL = `${__ENV.FLOW_BLOB_URL}` + "/" + fileName + `${__ENV.ACCESS_SIGNATURE_QUERY_STRING}`;
			

			let params = {
				headers: {
					'x-ms-blob-type': 'BlockBlob',
				},
			};
			
			const payload = {
    file: http.file(file_to_load, fileName),
  };

			let r = http.put(flowBlobURL, payload, params);

			console.log("SETUP - flowBlobURL: " + flowBlobURL + ",fileName: " + fileName + ",Status: " + r.status);

			check(r, {
				'status is 201': (r) => r.status === 201,
			});
		}
	}

	const data = {
		creditor_institution_codes: creditor_institution_codes,
		flow_ids: flow_ids,
		uploaded_files: uploaded_files,
		flow_dates: flow_dates
		
	};

	return { data: data };
}



export default function(data) {
	let creditor_institution_codes = data.data.creditor_institution_codes;
	let flow_ids = data.data.flow_ids;
	let flow_dates = data.data.flow_dates;
	
	getFlow(creditor_institution_codes, flow_ids, flow_dates);
}

function getFlow(creditor_institution_codes, flow_ids, flow_dates) {

	let urlBasePath = `${__ENV.BASE_URL}`

	// getFlow with random creditor_institution_code, flow_ids and flow_dates retrieval from the array
	let randomCreditorInstitutionCode = creditor_institution_codes[Math.floor(Math.random() * creditor_institution_codes.length)];
	let randomFlowId = flow_ids[Math.floor(Math.random() * flow_ids.length)];
	let randomFlowDate = flow_dates[Math.floor(Math.random() * flow_dates.length)];

	let url = `${urlBasePath}/organizations/${randomCreditorInstitutionCode}/reportings/${randomFlowId}/date/${randomFlowDate}`;

	let r = http.get(url);

	console.log("FUNCTION - getFlowURL " + url + "; Status " + r.status);

	check(r, {
		'status is 200': (r) => r.status === 200
	});
}

export function teardown(data) {
	// teardown code
	let files_to_delete = data.data.uploaded_files;
	for (let y = 0; y < files_to_delete.length; y++) {

			let flowBlobURL = `${__ENV.FLOW_BLOB_URL}` + "/" + files_to_delete [y] + `${__ENV.ACCESS_SIGNATURE_QUERY_STRING}`;


			let r = http.del(flowBlobURL, null, null);

			console.log("TEARDOWN - flowBlobURL " + flowBlobURL + "; Status " + r.status);

			check(r, {
				'status is 202': (r) => r.status === 202,
			});
	}
}