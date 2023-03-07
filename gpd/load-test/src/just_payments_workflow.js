import http from 'k6/http';
import { check } from 'k6';
import { SharedArray } from 'k6/data';
import { makeidMix, randomString, getPayload, getRandomItemFromArray } from './modules/helpers.js';

//k6 run -o influxdb=http://influxdb:8086/k6 -e BASE_URL=http://localhost:8085 gpd/load-test/src/payments_workflow.js
export let options = JSON.parse(open(__ENV.TEST_TYPE));

const varsArray = new SharedArray('vars', function () {
    return JSON.parse(open(`${__ENV.VARS}`)).environment;
});

// workaround to use shared array (only array should be used)
const vars = varsArray[0];
const rootUrl = `${vars.host}`;

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

	let numberOfEventsToPreload = 100;

	for (let i = 0; i < numberOfEventsToPreload; i++) {
        const creditor_institution_code = randomString(11, "0123456789");
        const iupd = makeidMix(35);
        const iuv = makeidMix(35);
        const due_date = new Date().addDays(30);
        const retention_date = new Date().addDays(90);
        const transfer_id = '1';
        var url = `${rootUrl}/organizations/${creditor_institution_code}/debtpositions`;
        var payload = getPayload(iupd, iuv, due_date, retention_date, transfer_id);
        var r = http.post(url, payload, params);
        check(r, {'CreateDebtPosition status is 201': (r) => r.status === 201,});
        url = `${rootUrl}/organizations/${creditor_institution_code}/debtpositions/${iupd}/publish`;
        r = http.post(url, params);
        check(r, {'PublishDebtPosition status is 200': (r) => r.status === 200,});

        pdArray.push([creditor_institution_code, iuv]);
	}

	 return { pds: pdArray }

	 // precondition is moved to default fn because in this stage
	 // __VU is always 0 and cannot be used to create env properly
}

export default function(data) {
      let pair = getRandomItemFromArray(data.pds);

      creditor_institution_code = pair[0];
      iuv = pair[1];

      // Pay Payment Option
      url = `${rootUrl}/organizations/${creditor_institution_code}/paymentoptions/${iuv}/pay`;

      payload = JSON.stringify(
        {
          "paymentDate": new Date(),
          "paymentMethod": "bonifico",
          "pspCompany": "Intesa San Paolo",
          "idReceipt": "TRN123456789"
        }
      );

      r = http.post(url, payload, params);

      console.log("PayPaymentOption call - creditor_institution_code = " + creditor_institution_code + ", iuv = " + iuv + ", Status = " + r.status);

      check(r, {
        'PayPaymentOption status is 200': (r) => r.status === 200,
      });
}
