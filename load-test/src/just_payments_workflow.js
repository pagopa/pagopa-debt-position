import http from 'k6/http';
import exec from 'k6/execution';
import { check } from 'k6';
import { SharedArray } from 'k6/data';
import { Counter } from 'k6/metrics';
import { makeidNumber, makeidMix, randomString, getPayload, getRandomItemFromArray } from './modules/helpers.js';
import { getDebtPosition } from "./modules/data.js";

//k6 run -o influxdb=http://influxdb:8086/k6 -e BASE_URL=http://localhost:8080 gpd/load-test/src/payments_workflow.js
export let options = JSON.parse(open(__ENV.TEST_TYPE));

const varsArray = new SharedArray('vars', function () {
    return JSON.parse(open(`${__ENV.VARS}`)).environment;
});

// workaround to use shared array (only array should be used)
const vars = varsArray[0];
const rootUrl = `${vars.host}`;
const numberOfPositionsToPreload = __ENV.PRELOAD_PD_NUMBER;
const batchSize = 150;

const payCounter = new Counter('pay_counter');

const gpdParams = {
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
    let tag;
    const numberOfBatch = Math.ceil(numberOfPositionsToPreload / batchSize);

    for (let i = 0; i < numberOfBatch; i++) {
        let batchArrayDebtPosition = new Array();

        for (let j = 0; j < batchSize; j++) {
            const creditorInstitutionCode = randomString(11, "0123456789");
            const iupd = makeidMix(35);
            const iuv_1 = makeidMix(35);
            const iuv_2 = makeidNumber(17);
            const iuv_3 = makeidNumber(17);
            const due_date = new Date().addDays(30);
            const retention_date = new Date().addDays(90);
            const transfer_id_1 = "1";
            const transfer_id_2 = "2";

            pdArray.push([creditorInstitutionCode, iuv_1]);

            let url = `${rootUrl}/organizations/${creditorInstitutionCode}/debtpositions?toPublish=true`;
            let payload = getDebtPosition(iupd, iuv_1, iuv_2, iuv_3, due_date, retention_date, transfer_id_1, transfer_id_2);
            batchArrayDebtPosition.push(["POST", url, payload, gpdParams]);
        }

        let responses = http.batch(batchArrayDebtPosition);
        for (let j = 0; j < batchSize; j++) {
          check(responses[j], { "Create and Publish DebtPosition status is 201": (response) => response.status === 201 }, { paymentRequest: "CreateDebtPosition" });
        }
    }

    console.log(pdArray);
    return { pds: pdArray };
    // precondition is moved to default fn because in this stage
    // __VU is always 0 and cannot be used to create env properly
}

export default function(data) {
    let idx = exec.scenario.iterationInInstance;
    let pair = data.pds[idx];

    const creditorInstitutionCode = pair[0];
    const iuv = pair[1];

    // Pay Payment Option
    const url = `${rootUrl}/organizations/${creditorInstitutionCode}/paymentoptions/${iuv}/pay`;

    const payload = JSON.stringify(
    {
      "paymentDate": new Date(),
      "paymentMethod": "bonifico",
      "pspCompany": "Intesa San Paolo",
      "idReceipt": "TRN123456789"
    }
    );

    const r = http.post(url, payload, gpdParams);

    if(r.status != 200)
        console.log('Error ' + r.status_text + ' when call ' + r.url);

    check(r, {'PayPaymentOption status is 200': (r) => r.status === 200, });

    payCounter.add(1);
}
