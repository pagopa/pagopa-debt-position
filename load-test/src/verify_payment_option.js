import http from 'k6/http';
import exec from 'k6/execution';
import { check } from 'k6';
import { SharedArray } from 'k6/data';
import { Counter } from 'k6/metrics';
import { makeidNumber, makeidMix, randomString } from './modules/helpers.js';

export let options = JSON.parse(open(__ENV.TEST_TYPE));

options.thresholds = {
	'http_req_duration{name:POST /payment-options/organizations/{organizationFiscalCode}/notices/{nav}}': [
		'p(95)<500',
		'p(99)<1000',
	],
};

const varsArray = new SharedArray('vars', function() {
	return JSON.parse(open(`${__ENV.VARS}`)).environment;
});

const vars = varsArray[0];
const rootOrgUrl = `${vars.host}`;
const rootInternalUrl = `${vars.host_internal}`;
const numberOfPositionsToPreload = __ENV.PRELOAD_PD_NUMBER;
const batchSize = 150;

const verifyCounter = new Counter('verify_counter');

const createParams = {
	headers: {
		'Content-Type': 'application/json',
		'Accept': 'application/json',
		'Ocp-Apim-Subscription-Key': __ENV.API_SUBSCRIPTION_KEY,
	},
};

const verifyParams = {
	headers: {
		'Content-Type': 'application/json',
		'Accept': 'application/json',
		'Ocp-Apim-Subscription-Key': __ENV.API_SUBSCRIPTION_KEY,
	},
	tags: {
		// metrics dedicated to verifyPaymentOption
		name: 'POST /payment-options/organizations/{organizationFiscalCode}/notices/{nav}',
	},
};

let noticesArray = [];

/**
 *  Preloads N debit positions v3 (only one paymentOption / installment)
 *  and stores [organizationFiscalCode, nav] pairs to call verifyPaymentOption
 */
export function setup() {
	const numberOfBatch = Math.ceil(numberOfPositionsToPreload / batchSize);
	let created = 0;

	for (let i = 0; i < numberOfBatch && created < numberOfPositionsToPreload; i++) {
		let batchArrayDebtPosition = [];

		for (let j = 0; j < batchSize && created < numberOfPositionsToPreload; j++) {
			const organizationFiscalCode = randomString(11, '0123456789');

			const iupd = `IUPD-V1-${makeidMix(24)}`;
			const nav = '3' + makeidNumber(17);   // es. "300000000000000001"
			const iuv = makeidNumber(17);   // es. "10000000000000001"

			const dueDate = new Date().addDays(30).toISOString();
			const retentionDate = new Date().addDays(90).toISOString();

			const payload = JSON.stringify({
			  iupd: iupd,
			  type: 'F',
			  payStandIn: true,
			  fiscalCode: 'RSSMRA80A01H501Z',
			  fullName: 'Mario Rossi',
			  streetName: 'Via Roma',
			  civicNumber: '10',
			  postalCode: '00100',
			  city: 'Roma',
			  province: 'RM',
			  region: 'Lazio',
			  country: 'IT',
			  email: 'mario.rossi@example.com',
			  phone: '+39061234567',
			  switchToExpired: false,
			  companyName: 'Comune di Esempio',
			  officeName: 'Ufficio Entrate',
			  validityDate: null,
			  paymentOption: [
			    {
			      nav: nav,
			      iuv: iuv,
			      amount: 10000,
			      description: "Pagamento in un'unica soluzione",
			      isPartialPayment: false,
			      dueDate: dueDate,
			      retentionDate: retentionDate,
			      fee: 0,
			      transfer: [
			        {
			          idTransfer: '1',
			          amount: 10000,
			          organizationFiscalCode: '12345678901',
			          remittanceInformation: 'TARI 2026 - saldo',
			          category: '010110',
			          iban: 'IT60X0542811101000000123456',
			          companyName: 'Comune di Esempio',
			          transferMetadata: [
			            { key: 'capitolo', value: 'TARI' },
			          ],
			        },
			      ],
			      paymentOptionMetadata: [
			        { key: 'note', value: 'Saldo unico' },
			      ],
			    },
			  ],
			});


			const url = `${rootOrgUrl}/organizations/${organizationFiscalCode}/debtpositions?toPublish=true&serviceType=GPD`;
			batchArrayDebtPosition.push(['POST', url, payload, createParams]);

			// stores the NAV that will be used in the verify call
			noticesArray.push([organizationFiscalCode, nav]);
			created++;
		}

		const responses = http.batch(batchArrayDebtPosition);
		for (let r of responses) {
			check(r, {
				'CreateDebtPosition status is 201': (res) => res.status === 201,
			});
			if (r.status !== 201) {
				console.log(`Error ${r.status} on create debt position: ${r.body}`);
			}
		}
	}

	console.log(`Preload ${noticesArray.length} positions for verifyPaymentOption`);
	return { notices: noticesArray };
}

export default function (data) {
  const notices = data.notices;
  const length = notices.length;

  if (length === 0) {
    return;
  }

  const globalIdx = exec.scenario.iterationInTest;
  const idx = globalIdx % length;

  const [organizationFiscalCode, nav] = notices[idx];

  // verifyPaymentOption: /payment-options/organizations/{organizationFiscalCode}/notices/{nav}
  const url = `${rootInternalUrl}/payment-options/organizations/${organizationFiscalCode}/notices/${nav}`;
  const r = http.post(url, null, verifyParams);

  if (r.status !== 200) {
    console.log(
      `Error ${r.status} calling verifyPaymentOption for org=${organizationFiscalCode}, nav=${nav} - body=${r.body}`,
    );
  }

  check(r, { 'VerifyPaymentOption status is 200': (res) => res.status === 200 });

  verifyCounter.add(1);
}

