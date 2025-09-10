import http from 'k6/http';
import { check } from 'k6';
import { SharedArray } from 'k6/data';
import { makeidMix, randomString } from './modules/helpers.js';

// Carico opzioni dal file passato in TEST_TYPE
export let options = JSON.parse(open(__ENV.TEST_TYPE));

// Override JSON thresholds
options.thresholds = {
  'http_req_duration{name:POST /debtpositions}': ['p(95)<1500', 'p(99)<2000'],
  'http_req_duration{name:PUT /paymentoptions/{iuv}/notificationfee}': ['p(95)<1500', 'p(99)<2000'],
  'http_req_duration{name:POST /debtpositions/{iupd}/publish}': ['p(95)<1500', 'p(99)<2000'],
  'http_req_duration{name:POST /paymentoptions/{iuv}/pay}': ['p(95)<1500', 'p(99)<2000'],
  'http_req_duration{name:POST /paymentoptions/{iuv}/transfers/{id}/report}': ['p(95)<1500', 'p(99)<2000'],
  'http_req_duration{name:GET /paymentoptions/{iuv}}': ['p(95)<1500', 'p(99)<2000'],
};

const varsArray = new SharedArray('vars', function () {
  return JSON.parse(open(`${__ENV.VARS}`)).environment;
});

// workaround to use shared array (only array should be used)
const vars = varsArray[0];
const rootUrl = `${vars.host}`;

// helper per params con tag name
function reqParams(name) {
  return {
    headers: {
      'Content-Type': 'application/json',
      'Ocp-Apim-Subscription-Key': __ENV.API_SUBSCRIPTION_KEY,
    },
    tags: { name }, // distinct metric per request
  };
}

export default function () {
  const creditor_institution_code = randomString(11, '0123456789');
  const iupd = makeidMix(35);
  const iuv = makeidMix(35);
  const due_date = new Date(Date.now() + 30*24*60*60*1000);
  const retention_date = new Date(Date.now() + 90*24*60*60*1000);
  const transfer_id = '1';

  // ----- STEP 1: Create new debt position -----
  let url = `${rootUrl}/organizations/${creditor_institution_code}/debtpositions`;
  let payload = JSON.stringify({
    iupd: iupd,
    type: 'F',
    fiscalCode: 'JHNDOE00A01F205N',
    fullName: 'John Doe',
    streetName: 'streetName',
    civicNumber: '11',
    postalCode: '00100',
    city: 'city',
    province: 'RM',
    region: 'RM',
    country: 'IT',
    email: 'lorem@lorem.com',
    phone: '333-123456789',
    switchToExpired: false,
    companyName: 'companyName',
    officeName: 'officeName',
    paymentOption: [
      {
        iuv: iuv,
        amount: 10000,
        description: 'Canone Unico Patrimoniale - CORPORATE',
        isPartialPayment: false,
        dueDate: due_date,
        retentionDate: retention_date,
        fee: 0,
        transfer: [
          {
            idTransfer: transfer_id,
            amount: 10000,
            remittanceInformation: 'remittanceInformation 1',
            category: '9/0101108TS/',
            iban: 'IT0000000000000000000000000',
          },
        ],
      },
    ],
  });

  let r = http.post(url, payload, reqParams('POST /debtpositions'));
  //console.log(`CreateDebtPosition: ${r.timings.duration} ms (status ${r.status})`);

  check(r, { 'CreateDebtPosition status is 201': (r) => r.status === 201 });
  if (r.status !== 201) return;

  // ----- STEP 2: Update notification fee -----
  url = `${rootUrl}/organizations/${creditor_institution_code}/paymentoptions/${iuv}/notificationfee`;
  payload = JSON.stringify({ notificationFee: 150 });
  r = http.put(url, payload, reqParams('PUT /paymentoptions/{iuv}/notificationfee'));
  //console.log(`UpdateNotificationFee: ${r.timings.duration} ms (status ${r.status})`);

  check(r, { 'UpdateNotificationFee status is 200/209': (r) => r.status === 200 || r.status === 209 });
  if (r.status !== 200 && r.status !== 209) return;

  // ----- STEP 3: Publish debt position -----
  url = `${rootUrl}/organizations/${creditor_institution_code}/debtpositions/${iupd}/publish`;
  r = http.post(url, null, reqParams('POST /debtpositions/{iupd}/publish')); // body null!
  //console.log(`PublishDebtPosition: ${r.timings.duration} ms (status ${r.status})`);

  check(r, { 'PublishDebtPosition status is 200': (r) => r.status === 200 });
  if (r.status !== 200) return;

  // ----- STEP 4: Pay payment option -----
  url = `${rootUrl}/organizations/${creditor_institution_code}/paymentoptions/${iuv}/pay`;
  payload = JSON.stringify({
    paymentDate: new Date(),
    paymentMethod: 'bonifico',
    pspCompany: 'Intesa San Paolo',
    idReceipt: 'TRN123456789',
  });
  r = http.post(url, payload, reqParams('POST /paymentoptions/{iuv}/pay'));
  //console.log(`PayPaymentOption: ${r.timings.duration} ms (status ${r.status})`);

  check(r, { 'PayPaymentOption status is 200': (r) => r.status === 200 });
  if (r.status !== 200) return;

  // ----- STEP 5: Report transfer -----
  url = `${rootUrl}/organizations/${creditor_institution_code}/paymentoptions/${iuv}/transfers/${transfer_id}/report`;
  r = http.post(url, null, reqParams('POST /paymentoptions/{iuv}/transfers/{id}/report')); // body null!
  //console.log(`ReportTransfer: ${r.timings.duration} ms (status ${r.status})`);

  check(r, { 'ReportTransfer status is 200': (r) => r.status === 200 });
  if (r.status !== 200) return;

  // ----- STEP 6: Get payment option -----
  url = `${rootUrl}/organizations/${creditor_institution_code}/paymentoptions/${iuv}`;
  r = http.get(url, reqParams('GET /paymentoptions/{iuv}'));
  //console.log(`GetOrganizationPaymentOption: ${r.timings.duration} ms (status ${r.status})`);

  check(r, { 'GetOrganizationPaymentOption status is 200': (r) => r.status === 200 });
  check(r, {
    'GetOrganizationPaymentOption payment option status is reported': (r) =>
      JSON.parse(r.body).status === 'PO_REPORTED',
  });
  check(r, {
    'GetOrganizationPaymentOption payment option iupd is present and valued': (r) =>
      JSON.parse(r.body).iupd === iupd,
  });
}

// --------- final file summary ---------
/*
export function handleSummary(data) {
  const byName = {};
  for (const [k, v] of Object.entries(data.metrics)) {
    if (k.startsWith('http_req_duration{') && k.includes('name:')) {
      byName[k] = v;
    }
  }
  const summary = {
      http_req_duration_by_name: byName,
  };
  return {
      'summary.json': JSON.stringify(summary, null, 2),   // file
      stdout: JSON.stringify(summary, null, 2),           // console
  };
}*/