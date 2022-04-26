import http from 'k6/http';
import { check } from 'k6';
import { randomString } from './modules/helpers.js';

export let options = {
  summaryTrendStats: ['avg', 'min', 'med', 'max', 'p(95)', 'p(99)', 'p(99.99)', 'count'],
  stages: [
    { duration: '1m', target: 50 }, // simulate ramp-up of traffic from 1 to 50 users over 1 minutes.
  ],
  thresholds: {
    http_req_failed: ['rate<0.01'], // http errors should be less than 1%
    http_req_duration: ['p(99)<1500'], // 99% of requests must complete below 1500ms
  },
};



export default function () {

  var urlBasePath = `${__ENV.BASE_URL}`

  const creditor_institution_code = randomString(11, "0123456789");
  const flow_id = "2022-01-12PPAYITR1XXX-S239349322";
  const date = new Date();

  var url = `${urlBasePath}/api/organizations/${creditor_institution_code}/reportings/${flow_id}/date/${date}`;

  var r = http.get(url);

  console.log("creditor_institution_code " + creditor_institution_code + " Status " + r.status);

  check(r, {
    'status is 200': (r) => r.status === 200,
  });


}
