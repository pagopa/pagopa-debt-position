import http from 'k6/http';
import { check } from 'k6';
import { SharedArray } from 'k6/data';
import { makeidMix, randomString } from './modules/helpers.js';

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

  var url = `${rootUrl}/organizations/${creditor_institution_code}/debtpositions`;

  const iupd = makeidMix(35);
  const iuv = makeidMix(35);
  const due_date = new Date().addDays(30);
  const retention_date = new Date().addDays(90);
  const validity_date = new Date().addDays(10);

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
      "validityDate": validity_date,
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
              "idTransfer": "1",
              "amount": 8000,
              "remittanceInformation": "remittanceInformation 1",
              "category": "9/0101108TS/",
              "iban": "IT0000000000000000000000000"
            },
            {
              "idTransfer": "2",
              "amount": 2000,
              "remittanceInformation": "remittanceInformation 2",
              "category": "9/0101108TS/",
              "iban": "IT0000000000000000000000000"
            }
          ]
        }
      ]
    }
  );

  var r = http.post(url, payload, params);

  //console.log("creditor_institution_code " + creditor_institution_code + " Status " + r.status);

  check(r, {
    'status is 201': (r) => r.status === 201,
  });


}
