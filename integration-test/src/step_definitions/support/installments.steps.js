const { Given, When, Then, AfterAll, Before, BeforeAll, setDefaultTimeout} = require('@cucumber/cucumber')
const { gpdSessionBundle, gpdPayBundle } = require('./utility/data');
const { createDebtPosition } = require('./clients/gpd_client');
const { addDays, makeidNumber} = require('./utility/helpers');
const { executePaymentOptionGetByIuv, executePaymentOptionPay, executeDebtPositionDeletion } = require('./logic/gpd_logic');

let idOrg = process.env.organization_fiscal_code;

/*
 *  Get (Activate) the installment payment option
 */
When('we get the installment {int} of payment option {int}', async function (installmentIndex, optionIndex) {
    const optIdx = Number(optionIndex) - 1;
    const instIdx = Number(installmentIndex) - 1;

    const created = gpdSessionBundle.createdDebtPosition || gpdSessionBundle.debtPosition || {};
    const paymentOptions = created.paymentOption || [];

    if (!paymentOptions[optIdx] || !paymentOptions[optIdx].installments || !paymentOptions[optIdx].installments[instIdx]) {
        throw new Error(`Installment ${installmentIndex} of payment option ${optionIndex} not found`);
    }

    await executePaymentOptionGetByIuv(gpdPayBundle, idOrg, paymentOptions[optIdx].installments[instIdx].nav);
});

/*
 *  Paying the installment payment option
 */
When('the installment {int} of payment option {int} is paid', async function (installmentIndex, optionIndex) {
    const optIdx = Number(optionIndex) - 1;
    const instIdx = Number(installmentIndex) - 1;

    const created = gpdSessionBundle.createdDebtPosition || gpdSessionBundle.debtPosition || {};
    const paymentOptions = created.paymentOption || [];

    if (!paymentOptions[optIdx] || !paymentOptions[optIdx].installments || !paymentOptions[optIdx].installments[instIdx]) {
        throw new Error(`Installment ${installmentIndex} of payment option ${optionIndex} not found`);
    }

    return await executePaymentOptionPay(gpdPayBundle, idOrg, paymentOptions[optIdx].installments[instIdx].nav)
});

When('the multi debt position is created using {string} API with the following installments:', async function (apiVersion, table) {
    const rows = table.hashes();

    // group installments by option preserving order
    const grouped = new Map();
    for (const r of rows) {
        const opt = (r['option'] || r['option ']).toString().trim();
        const notice = (r['notice_number'] || r['notice_number ' ] || r['notice number'] || r['notice_number']).toString().trim();
        if (!grouped.has(opt)) grouped.set(opt, []);
        grouped.get(opt).push(notice);
    }

    // build v3 payload's paymentOption array
    const paymentOption = [];
    for (const [opt, notices] of grouped.entries()) {
        const installments = notices.map(n => {
            const iuvToAssign = (String(n).trim() === 'random') ? makeidNumber(17) : n;

            return {
                iuv: iuvToAssign,
                amount: (gpdSessionBundle.debtPosition.amount ? gpdSessionBundle.debtPosition.amount * 100 : 30000),
                description: "Installment",
                dueDate: gpdSessionBundle.debtPosition.dueDate || addDays(30),
                transfer: [
                    {
                        idTransfer: gpdSessionBundle.debtPosition.transferId1 || '1',
                        amount: Math.floor((gpdSessionBundle.debtPosition.amount ? gpdSessionBundle.debtPosition.amount * 100 : 30000) / 3),
                        remittanceInformation: 'Rata 1',
                        category: '9/0101108TS/',
                        iban: gpdSessionBundle.debtPosition.iban || 'mockIban'
                    },
                    {
                        idTransfer: gpdSessionBundle.debtPosition.transferId2 || '2',
                        organizationFiscalCode: gpdSessionBundle.debtPosition.transferOtherCIFiscalCode || undefined,
                        amount: (Math.floor((gpdSessionBundle.debtPosition.amount ? gpdSessionBundle.debtPosition.amount * 100 : 30000) / 3) * 2),
                        remittanceInformation: 'Rata 2',
                        category: '9/0101108TS/',
                        iban: gpdSessionBundle.debtPosition.iban || 'mockIban'
                    }
                ]
            };
        });

        paymentOption.push({
            validityDate: gpdSessionBundle.debtPosition.validityDate,
            retentionDate: gpdSessionBundle.debtPosition.retentionDate,
            switchToExpired: false,
            debtor: {
                type: 'F',
                fiscalCode: gpdSessionBundle.payer.fiscalCode,
                fullName: gpdSessionBundle.payer.fullName,
                streetName: gpdSessionBundle.payer.streetName,
                civicNumber: gpdSessionBundle.payer.civicNumber,
                postalCode: gpdSessionBundle.payer.postalCode,
                city: gpdSessionBundle.payer.city,
                province: gpdSessionBundle.payer.province,
                region: gpdSessionBundle.payer.region,
                country: gpdSessionBundle.payer.country,
                email: gpdSessionBundle.payer.email,
                phone: gpdSessionBundle.payer.phone
            },
            installments
        });
    }

    const body = {
        iupd: gpdSessionBundle.debtPosition.iupd,
        companyName: gpdSessionBundle.payer.companyName,
        officeName: gpdSessionBundle.payer.officeName,
        paymentOption
    };

    const response = await createDebtPosition(idOrg, body, undefined, false, apiVersion);
    gpdSessionBundle.responseToCheck = response;
    gpdSessionBundle.createdDebtPosition = response.data;
});