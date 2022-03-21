import http from 'k6/http';
import { check } from 'k6';
import { makeidMix, randomString } from './modules/helpers.js';

export let options = {
  summaryTrendStats: ['avg', 'min', 'med', 'max', 'p(95)', 'p(99)', 'p(99.99)', 'count'],
  stages: [
    { duration: '1m', target: 50 }, // simulate ramp-up of traffic from 1 to 50 users over 1 minutes.
  ],
  thresholds: {
    http_req_failed: ['rate<0.01'], // http errors should be less than 1%
    http_req_duration: ['p(99)<1500'], // 99% of requests must complete below 1500ms
    'http_req_duration{gpdMethod:CreateDebtPosition}': ['p(95)<1000'], // threshold on creation API requests only
    'http_req_duration{gpdMethod:PublishDebtPosition}': ['p(95)<1000'], // threshold on publication API requests only
    'http_req_duration{gpdMethod:PayPaymentOption}': ['p(95)<1000'], // threshold on payment API requests only
    'http_req_duration{gpdMethod:ReportTransfer}': ['p(95)<1000'], // threshold on report API requests only
    'http_req_duration{gpdMethod:GetOrganizationPaymentOption}': ['p(95)<1000'], // threshold on get API requests only
  },

};



export default function () {

  var urlBasePath = `${__ENV.BASE_URL}`

  const creditor_institution_code = randomString(11, "0123456789");
  const iupd = makeidMix(35);
  const iuv = makeidMix(35);
  const due_date = new Date().addDays(30);
  const retention_date = new Date().addDays(90);
  const transfer_id = '01';

  // Create new debt position (no validity date).
  var tag = {
    gpdMethod: "CreateDebtPosition",
  };
  

  var url = `${urlBasePath}/organizations/${creditor_institution_code}/debtpositions`;

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


  var params = {
    headers: {
      'Content-Type': 'application/json'
    },
  };

  var r = http.post(url, payload, params);

  console.log("CreateDebtPosition call - creditor_institution_code = " + creditor_institution_code + ", Status = " + r.status);

  check(r, {
    'CreateDebtPosition status is 201': (r) => r.status === 201,
  }, tag);

  // if the debt position has been correctly created => publish 
  if (r.status === 201) {
    // sleep(1);
    // Publish the debt position.
    tag = {
      gpdMethod: "PublishDebtPosition",
    };
    url = `${urlBasePath}/organizations/${creditor_institution_code}/debtpositions/${iupd}/publish`;

    r = http.post(url, params);

    console.log("PublishDebtPosition call - creditor_institution_code = " + creditor_institution_code + ", iupd = " + iupd + ", Status = " + r.status);

    check(r, {
      'PublishDebtPosition status is 200': (r) => r.status === 200,
    }, tag);

    // if the debt position has been correctly published => pay 
    if (r.status === 200) {
      // sleep(1);
      // Pay Payment Option.
      tag = {
        gpdMethod: "PayPaymentOption",
      };

      url = `${urlBasePath}/organizations/${creditor_institution_code}/paymentoptions/${iuv}/pay`;

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
      }, tag);

      // if the payment option has been correctly paid => report
      if (r.status === 200) {
        // sleep(1);
        // Report Transfer.
        tag = {
          gpdMethod: "ReportTransfer",
        };
        url = `${urlBasePath}/organizations/${creditor_institution_code}/paymentoptions/${iuv}/transfers/${transfer_id}/report`;

        r = http.post(url, params);

        console.log("ReportTransfer call - creditor_institution_code = " + creditor_institution_code + ", iuv = " + iuv + ", transfer_id = " + transfer_id + ", Status = " + r.status);

        check(r, {
          'ReportTransfer status is 200': (r) => r.status === 200,
        }, tag);

        // if the transfer has been correctly reported => get
        if (r.status === 200) {
          // Get details of a specific payment option.
          tag = {
            gpdMethod: "GetOrganizationPaymentOption",
          };
          url = `${urlBasePath}/organizations/${creditor_institution_code}/paymentoptions/${iuv}`;

          r = http.get(url, params);

          console.log("GetOrganizationPaymentOption call - creditor_institution_code = " + creditor_institution_code + ", iuv = " + iuv + ", Status = " + r.status);

          check(r, {
            'GetOrganizationPaymentOption status is 200': (r) => r.status === 200,
          }, tag);

          check(r, {
            'GetOrganizationPaymentOption payment option status is reported': (r) => (JSON.parse(r.body)).status === 'PO_REPORTED',
          }, tag);

        }
      }
    }
 
    // sleep(2);
  }


}
