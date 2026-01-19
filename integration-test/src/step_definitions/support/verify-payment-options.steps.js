const { Given, When, Then, setDefaultTimeout } = require('@cucumber/cucumber');
const assert = require('assert');

const { executeHealthCheckForGPD } = require('./logic/health_checks_logic');
const { createDebtPosition } = require('./clients/gpd_client');
const { gpdSessionBundle } = require('./utility/data');
const { executeVerifyPaymentOptions } = require('./logic/gpd_logic');
const Data = require('./utility/data');
const { addDays } = require('./utility/helper');

setDefaultTimeout(30000);

// State shared between steps
let ctx = {
  lastResponse: null,
};

// ---------- STEPS ----------


/*
 *  Precondition: create a PD V3 with single + plan
 */
Given(
  'a V3 debt position with one single-payment and one 2-installment plan exists for organization {string}',
  async function (org) {
    const payload = Data.v3.singleAndPlan(org);

    if (
      payload &&
      payload.paymentOption &&
      payload.paymentOption[0] &&
      payload.paymentOption[0].installments &&
      payload.paymentOption[0].installments[0]
    ) {
      ctx.singleNav = payload.paymentOption[0].installments[0].nav;
    }

    const res = await createDebtPosition(org, payload, undefined, false, 'v3');
    assert.ok([200, 201, 409].includes(res.status), `Unexpected status: ${res.status} - ${JSON.stringify(res.data)}`);
  }
);

/*
 *  Call to the verifyPaymentOptions API
 */
When('I call verifyPaymentOptions for organization {string} with nav {string}', async function (org, nav) {
  const res = await executeVerifyPaymentOptions(gpdSessionBundle, org, nav);
  ctx.lastResponse = res || gpdSessionBundle.responseToCheck;
  if (!ctx.lastResponse) throw new Error('verifyPaymentOptions returned no response');
});

When(
  'I call verifyPaymentOptions for organization {string} with the single installment nav',
  async function (org) {
    if (!ctx.singleNav) {
      throw new Error('singleNav not set in context – make sure the Given step ran correctly');
    }
    const res = await executeVerifyPaymentOptions(gpdSessionBundle, org, ctx.singleNav);
    ctx.lastResponse = res || gpdSessionBundle.responseToCheck;
    if (!ctx.lastResponse) throw new Error('verifyPaymentOptions returned no response');
  }
);

/*
 *  Assertions
 */
Then('the HTTP status is {int}', function (code) {
  assert.ok(ctx.lastResponse, 'No HTTP response captured');
  assert.strictEqual(ctx.lastResponse.status, code, `Body: ${JSON.stringify(ctx.lastResponse.data)}`);
});

Then('the payload has at least {int} payment option groups', function (min) {
  const groups = (ctx.lastResponse.data && ctx.lastResponse.data.paymentOptions) || [];
  assert.ok(Array.isArray(groups), 'paymentOptions must be an array');
  assert.ok(groups.length >= min, `Expected at least ${min} groups, got ${groups.length}`);
});

Then('there is at least one group with 1 installment described as {string}', function (expected) {
  const groups = (ctx.lastResponse.data && ctx.lastResponse.data.paymentOptions) || [];
  const singles = groups.filter(g => g.numberOfInstallments === 1);
  assert.ok(singles.length >= 1, 'No group with numberOfInstallments == 1');
  const descriptions = singles.map(g => g.description);
  assert.ok(descriptions.includes(expected), `Expected a single group description "${expected}", got: ${descriptions.join(', ')}`);
});

Then('there is at least one group with more than 1 installment described starting with {string}', function (prefix) {
  const groups = (ctx.lastResponse.data && ctx.lastResponse.data.paymentOptions) || [];
  const plans = groups.filter(g => (g.numberOfInstallments || 0) > 1);
  assert.ok(plans.length >= 1, 'No group with numberOfInstallments > 1');
  const ok = plans.some(g => typeof g.description === 'string' && g.description.startsWith(prefix));
  const got = plans.map(g => g.description).join(', ');
  assert.ok(ok, `No plan description starts with "${prefix}". Got: ${got}`);
});

Then('groups are ordered by ascending dueDate', function () {
  const groups = (ctx.lastResponse.data && ctx.lastResponse.data.paymentOptions) || [];
  const dates = groups.map(g => (g.dueDate ? new Date(g.dueDate).getTime() : Number.MAX_SAFE_INTEGER));
  for (let i = 1; i < dates.length; i++) {
    assert.ok(dates[i] >= dates[i - 1], `Groups are not ordered by ascending dueDate at index ${i - 1} -> ${i}`);
  }
});

Then('the response content-type contains {string}', function (expectedCT) {
  const ct = (ctx.lastResponse.headers && (ctx.lastResponse.headers['content-type'] || ctx.lastResponse.headers['Content-Type'])) || '';
  assert.ok(ct.includes(expectedCT), `Content-Type "${ct}" does not contain "${expectedCT}"`);
});

Then('there is at least one group with 1 installment and null po description', function () {
  const groups = (ctx.lastResponse.data && ctx.lastResponse.data.paymentOptions) || [];
  const singles = groups.filter(g => g.numberOfInstallments === 1);
  assert.ok(singles.length >= 1, 'No group with numberOfInstallments == 1');
  const hasNull = singles.some(g => g.description == null); // null o undefined
  const got = singles.map(g => g.description);
  assert.ok(hasNull, `Expected at least one single group with null description. Got: ${got.join(', ')}`);
});

Then('there is at least one group with more than 1 installment and null po description', function () {
  const groups = (ctx.lastResponse.data && ctx.lastResponse.data.paymentOptions) || [];
  const plans = groups.filter(g => (g.numberOfInstallments || 0) > 1);
  assert.ok(plans.length >= 1, 'No group with numberOfInstallments > 1');
  const hasNull = plans.some(g => g.description == null); // null o undefined
  const got = plans.map(g => g.description);
  assert.ok(hasNull, `Expected at least one plan group with null description. Got: ${got.join(', ')}`);
});


// ---------- UTILITIES ----------

function randomNav(maxDigits) {
  const len = Math.max(1, Math.min(maxDigits, 25)); // sicurezza
  let out = '';
  for (let i = 0; i < len; i++) {
    out += Math.floor(Math.random() * 10); // 0–9
  }
  return out;
}

// Build a PD V3 with:
// - 1 SINGLE option (1 installment) 
// - 1 2-installment plan
function buildPpV3SingleAndPlan(orgFiscalCode) {
  // NAV/IUV random generation
  const singleNav = randomNav(25);
  const planNav1  = randomNav(25);
  const planNav2  = randomNav(25);

  const pp = {
    iupd: `IUPD-E2E-VERIFY-${Math.floor(Math.random() * 1e9)}`,
    payStandIn: true,
    companyName: 'Comune di Esempio',
    officeName: 'Ufficio Entrate',
    paymentOption: [
      {
        description: "Pagamento in un'unica soluzione",
        validityDate: addDays(1),
        retentionDate: addDays(60),
        switchToExpired: false,
        debtor: {
          type: 'F',
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
        },
        installments: [
          {
            nav: singleNav,
            iuv: singleNav,
            amount: 1000,
            description: 'Saldo unico',
            dueDate: addDays(30),
            transfer: [
              {
                idTransfer: '1',
                amount: 1000,
                organizationFiscalCode: orgFiscalCode,
                remittanceInformation: 'TARI 2026 - saldo',
                category: '10/22252/20',
                iban: 'IT60X0542811101000000123456',
                companyName: 'Comune di Esempio',
                transferMetadata: [{ key: 'capitolo', value: 'TARI' }],
              },
            ],
            installmentMetadata: [{ key: 'note', value: 'Pagamento in unica soluzione' }],
          },
        ],
      },
      {
        description: 'Piano rateale A (2 rate)',
        validityDate: addDays(1),
        retentionDate: addDays(60),
        switchToExpired: true,
        debtor: {
          type: 'F',
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
        },
        installments: [
          {
            nav: planNav1,
            iuv: planNav1,
            amount: 600,
            description: 'Piano A - Rata 1/2',
            dueDate: addDays(45),
            transfer: [
              {
                idTransfer: '1',
                amount: 600,
                organizationFiscalCode: orgFiscalCode,
                remittanceInformation: 'TARI 2026 - Piano A R1',
                category: '10/22252/20',
                iban: 'IT60X0542811101000000123456',
                companyName: 'Comune di Esempio',
                transferMetadata: [
                  { key: 'piano', value: 'A' },
                  { key: 'rata', value: '1' },
                ],
              },
            ],
            installmentMetadata: [{ key: 'gruppo', value: 'PianoA' }],
          },
          {
            nav: planNav2,
            iuv: planNav2,
            amount: 400,
            description: 'Piano A - Rata 2/2',
            dueDate: addDays(75),
            transfer: [
              {
                idTransfer: '1',
                amount: 400,
                organizationFiscalCode: orgFiscalCode,
                remittanceInformation: 'TARI 2026 - Piano A R2',
                category: '10/22252/20',
                iban: 'IT60X0542811101000000123456',
                companyName: 'Comune di Esempio',
                transferMetadata: [
                  { key: 'piano', value: 'A' },
                  { key: 'rata', value: '2' },
                ],
              },
            ],
            installmentMetadata: [{ key: 'gruppo', value: 'PianoA' }],
          },
        ],
      },
    ],
  };

  return pp;
}


Data.v3 = Data.v3 || {};
Data.v3.singleAndPlan = buildPpV3SingleAndPlan;
