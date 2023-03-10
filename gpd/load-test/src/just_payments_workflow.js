import http from 'k6/http';
import exec from 'k6/execution';
import { check } from 'k6';
import { SharedArray } from 'k6/data';
import { Counter } from 'k6/metrics';
import { makeidMix, randomString, getPayload, getRandomItemFromArray } from './modules/helpers.js';

//k6 run -o influxdb=http://influxdb:8086/k6 -e BASE_URL=http://localhost:8085 gpd/load-test/src/payments_workflow.js
export let options = JSON.parse(open(__ENV.TEST_TYPE));

const varsArray = new SharedArray('vars', function () {
    return JSON.parse(open(`${__ENV.VARS}`)).environment;
});

// workaround to use shared array (only array should be used)
const vars = varsArray[0];
const rootUrl = `${vars.host}`;
const numberOfPositionsToPreload = __ENV.PRELOAD_PD_NUMBER;

const payCounter = new Counter('pay_counter');

const params = {
    headers: {
        'Content-Type': 'application/json',
        'Ocp-Apim-Subscription-Key': __ENV.API_SUBSCRIPTION_KEY
    },
};

var pdArray = new Array();

export function setup() {
	// 1. setup code (once)
	// The setup code runs, setting up the test environment (optional) and generating data
	// used to reuse code for the same VU

	for (let i = 0; i < numberOfPositionsToPreload; i++) {
        const creditor_institution_code = randomString(11, "0123456789");
        const iupd = makeidMix(35);
        const iuv = makeidMix(35);
        const due_date = new Date().addDays(30);
        const retention_date = new Date().addDays(90);
        const transfer_id = '1';
        var url = `${rootUrl}/organizations/${creditor_institution_code}/debtpositions`;
        var payload = getPayload(iupd, iuv, due_date, retention_date, transfer_id);
        var r = http.post(url, payload, params);
        url = `${rootUrl}/organizations/${creditor_institution_code}/debtpositions/${iupd}/publish`;
        r = http.post(url, params);

        pdArray.push([creditor_institution_code, iuv]);
	}

	 return { pds: pdArray }

	 // precondition is moved to default fn because in this stage
	 // __VU is always 0 and cannot be used to create env properly
}

export default function(data) {
    let idx = exec.instance.vusActive * exec.vu.iterationInScenario + exec.vu.idInInstance;
    let pair = data.pds[idx];

    const creditor_institution_code = pair[0];
    const iuv = pair[1];

    // Pay Payment Option
    const url = `${rootUrl}/organizations/${creditor_institution_code}/paymentoptions/${iuv}/pay`;

    const payload = JSON.stringify(
    {
      "paymentDate": new Date(),
      "paymentMethod": "bonifico",
      "pspCompany": "Intesa San Paolo",
      "idReceipt": "TRN123456789"
    }
    );

    const r = http.post(url, payload, params);

    check(r, {'PayPaymentOption status is 200': (r) => r.status === 200, });

    payCounter.add(1);
}
