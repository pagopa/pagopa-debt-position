import http from 'k6/http';
import { check } from 'k6';
import { SharedArray } from 'k6/data';
import { makeidMix, randomString } from './modules/helpers.js';

export let options = JSON.parse(open(__ENV.TEST_TYPE));

const varsArray = new SharedArray('vars', function() {
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

export default function() {

  // fixed value for the creditor_institution with multiple debt positions
  const creditor_institution_code = '80050050154'
  const iupd = makeidMix(35);
  const iuv = makeidMix(35);
  const due_date = new Date().addDays(1);
  const retention_date = new Date().addDays(90);
  const transfer_id = '1';

  // precondition: creation of a new debt position --> the GET of the list of debt positions returns at least one element
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
      "switchToExpired": false,
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

  check(r, {
    'CreateDebtPosition status is 201': (r) => r.status === 201,
  });


  // ----- NEXT STEP -----
  // if the debt position has been correctly created => get the list of organization debt positions by due_date
  if (r.status !== 201) return; // exit flow if failed
  
  let due_date_from = new Date().subDays(5).toISOString().split('T')[0];
  let due_date_to = new Date().addDays(5).toISOString().split('T')[0];
  
  url = `${rootUrl}/organizations/${creditor_institution_code}/debtpositions?limit=50&page=0&due_date_from=${due_date_from}&due_date_to=${due_date_to}&status=DRAFT&orderby=INSERTED_DATE&ordering=DESC`;

  r = http.get(url, params);

  check(r, {
    'GetOrganizationsList status is 200': (r) => r.status === 200,
  });

  check(r, {
    'GetOrganizationsList size is >= 1': (r) => (JSON.parse(r.body)).payment_position_list.length >= 1,
  });
}
