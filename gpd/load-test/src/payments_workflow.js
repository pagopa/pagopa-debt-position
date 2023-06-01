import http from 'k6/http';
import { check } from 'k6';
import { SharedArray } from 'k6/data';
import { makeidMix, randomString } from './modules/helpers.js';

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

export default function () {

  const creditor_institution_code = randomString(11, "0123456789");
  const iupd = makeidMix(35);
  const iuv = makeidMix(35);
  const due_date = new Date().addDays(30);
  const retention_date = new Date().addDays(90);
  const transfer_id = '1';

  // Create new debt position (no validity date).
  var url = `${rootUrl}/organizations/${creditor_institution_code}/debtpositions`;

  var payload = JSON.stringify(
    {
      "iupd": iupd,
      "type": "F",
      "fiscalCode": "JHNDOE00A01F205N",
      "fullName": "John Doe",
      "streetName": "streetName",
      "civicNumber": "11",
      "postalCode": "00100",
      "city": "city",
      "province": "RM",
      "region": "RM",
      "country": "IT",
      "email": "lorem@lorem.com",
      "phone": "333-123456789",
      "companyName": "companyName",
      "officeName": "officeName",
      "paymentOption": [
        {
          "iuv": iuv,
          "amount": 10000,
          "description": "Canone Unico Patrimoniale - CORPORATE",
          "isPartialPayment": false,
          "dueDate": due_date,
          "retentionDate": retention_date,
          "fee": 0,
          "transfer": [
            {
              "idTransfer": transfer_id,
              "amount": 10000,
              "remittanceInformation": "remittanceInformation 1",
              "category": "9/0101108TS/",
              "iban": "IT0000000000000000000000000"
            }
          ]
        }
      ]
    }
  );

  var r = http.post(url, payload, params);

  //console.log("CreateDebtPosition call - creditor_institution_code = " + creditor_institution_code + ", Status = " + r.status);
  check(r, {
    'CreateDebtPosition status is 201': (r) => r.status === 201,
  });


  // ----- NEXT STEP -----
  // if the debt position has been correctly created => update notification fee
  if (r.status !== 201) return; // exit flow if failed
  url = `${rootUrl}/organizations/${creditor_institution_code}/paymentoptions/${iuv}/notificationfee`;
  payload = JSON.stringify({
    "notificationFee": 150
  });
  r = http.put(url, payload, params);

  //console.log("UpdateNotificationFee call - creditor_institution_code = " + creditor_institution_code + ", iupd = " + iupd + ", Status = " + r.status);
  check(r, {
    'UpdateNotificationFee status is 200': (r) => r.status === 200,
  });


  // ----- NEXT STEP -----
  // if the debt position has been correctly created => publish 
  if (r.status !== 200) return; // exit flow if failed
  url = `${rootUrl}/organizations/${creditor_institution_code}/debtpositions/${iupd}/publish`;
  r = http.post(url, params);

  //console.log("PublishDebtPosition call - creditor_institution_code = " + creditor_institution_code + ", iupd = " + iupd + ", Status = " + r.status);
  check(r, {
    'PublishDebtPosition status is 200': (r) => r.status === 200,
  });


  // ----- NEXT STEP -----
  // if the debt position has been correctly published => pay 
  if (r.status !== 200) return; // exit flow if failed  

  // sleep(1);
  // Pay Payment Option.
  url = `${rootUrl}/organizations/${creditor_institution_code}/paymentoptions/${iuv}/pay`;
  payload = JSON.stringify({
    "paymentDate": new Date(),
    "paymentMethod": "bonifico",
    "pspCompany": "Intesa San Paolo",
    "idReceipt": "TRN123456789"
  });
  r = http.post(url, payload, params);

  //console.log("PayPaymentOption call - creditor_institution_code = " + creditor_institution_code + ", iuv = " + iuv + ", Status = " + r.status);
  check(r, {
    'PayPaymentOption status is 200': (r) => r.status === 200,
  });


  // ----- NEXT STEP -----
  // if the payment option has been correctly paid => report
  if (r.status !== 200) return; // exit flow if failed  

  // sleep(1);
  // Report Transfer.
  url = `${rootUrl}/organizations/${creditor_institution_code}/paymentoptions/${iuv}/transfers/${transfer_id}/report`;
  r = http.post(url, params);

  //console.log("ReportTransfer call - creditor_institution_code = " + creditor_institution_code + ", iuv = " + iuv + ", transfer_id = " + transfer_id + ", Status = " + r.status);
  check(r, {
    'ReportTransfer status is 200': (r) => r.status === 200,
  });


  // ----- NEXT STEP -----
  // if the transfer has been correctly reported => get
  if (r.status !== 200) return; // exit flow if failed  

  // Get details of a specific payment option.
  url = `${rootUrl}/organizations/${creditor_institution_code}/paymentoptions/${iuv}`;
  r = http.get(url, params);

  //console.log("GetOrganizationPaymentOption call - creditor_institution_code = " + creditor_institution_code + ", iuv = " + iuv + ", Status = " + r.status);
  check(r, {
    'GetOrganizationPaymentOption status is 200': (r) => r.status === 200,
  });

  //console.log( "Body: " + r.body);

  check(r, {
    'GetOrganizationPaymentOption payment option status is reported': (r) => (JSON.parse(r.body)).status === 'PO_REPORTED',
  });

  check(r, {
    'GetOrganizationPaymentOption payment option iupd is present and valued': (r) => (JSON.parse(r.body)).iupd === iupd,
  });

  // sleep(2);
}
