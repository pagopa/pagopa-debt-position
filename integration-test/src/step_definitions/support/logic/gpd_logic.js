const assert = require("assert");
const {
    readCreditorInstitution,
    readCreditorInstitutionBroker,
    readCreditorInstitutionIbans,
    readCIStationAssociation,
    readStation,
    readPSP,
    readChannel,
} = require("../clients/api_config_client");
const { createDebtPosition, getPaymentOptionDetail, publishDebtPosition, payDebtPosition } = require("../clients/gpd_client");
const { activatePaymentNotice, sendPaymentOutcome } = require("../clients/nodo_client");
const { verifyPaymentNotice, sendRT } = require("../clients/payments_client");
const { debugLog, makeidNumber, addDays, makeidMix, buildStringFromDate } = require("../utility/helpers");
const { 
    generateFirstName, 
    generateLastName, 
    generateRandomNumber, 
    generateCity, 
    generateStreet, 
    generateCompany, 
    generateFiscalCode
} = require("../utility/random_generator");
const { 
    buildActivatePaymentNoticeRequest,
    buildVerifyPaymentNoticeRequest,
    buildSendPaymentOutcomeRequest,
    buildSendRTRequest,
    buildPayRequest, 
} = require("../utility/request_builders");


async function retrievePaymentOptionDetail(bundle) {
    console.log(` - the client asks the detail for the analyzed debt positions with IUPD [${bundle.debtPosition.iupd}]..`);
    bundle.response = await getPaymentOptionDetail(bundle.creditorInstitution.id, bundle.debtPosition.iupd);
    debugLog(`Payment option retrieving API invocation returned HTTP status code: ${bundle.response.status} with body: ${JSON. stringify(bundle.response.data)}`);
}

async function generateDebtPosition(bundle, logStep) {
    if(logStep) {
        console.log(" - Given a not paid debt position..");
    }
    /* Retrieve and save CI, broker and station on bundle */
    await retrieveCreditorInstitution(bundle, process.env.creditor_institution_id);
    await retrieveBroker(bundle, process.env.broker_id);
    await retrieveStation(bundle, process.env.station_id);
    await retrievePSP(bundle, process.env.psp_id, process.env.channel_id);

    generateDebtor(bundle);
    generateDebtPositionDetail(bundle);

    let response = await createDebtPosition(bundle.creditorInstitution.id, bundle.debtPosition);
    debugLog(`Debt position creation API invocation returned HTTP status code: ${response.status} with body: ${JSON.stringify(response.data)}`);
    assert.strictEqual(response.status, 201);
    response = await publishDebtPosition(bundle.creditorInstitution.id, bundle.debtPosition.iupd);
    debugLog(`Debt position publishing API invocation returned HTTP status code: ${response.status} with body: ${JSON.stringify(response.data)}`);
    assert.strictEqual(response.status, 200);
}

async function generateAndPayDebtPosition(bundle) {
    console.log(" - Given a paid debt position..");
    await generateDebtPosition(bundle, false);
    let response = await verifyPaymentNotice(buildVerifyPaymentNoticeRequest(bundle));
    debugLog(`Payment notice verifying API invocation returned HTTP status code: ${response.status} with body: ${JSON. stringify(response.data)}`);
    assert.match(response.data, /<outcome>OK<\/outcome>/);
    
    response = await activatePaymentNotice(buildActivatePaymentNoticeRequest(bundle));
    debugLog(`Payment notice activation API invocation returned HTTP status code: ${response.status} with body: ${JSON. stringify(response.data)}`);
    assert.match(response.data, /<outcome>OK<\/outcome>/);
    let paymentTokenRegExp = /<paymentToken>([a-z0-9]+)<\/paymentToken>/
    bundle.debtPositionCalculation.paymentToken = paymentTokenRegExp.exec(response.data)?.[1];
    assert.ok(bundle.debtPositionCalculation.paymentToken !== undefined);

    response = await sendPaymentOutcome(buildSendPaymentOutcomeRequest(bundle));
    debugLog(`Payment outcome sending API invocation returned HTTP status code: ${response.status} with body: ${JSON. stringify(response.data)}`);
    assert.match(response.data, /<outcome>OK<\/outcome>/);
    
    response = await payDebtPosition(bundle.creditorInstitution.id, bundle.debtPosition.paymentOption[0].iuv, buildPayRequest(bundle));
    debugLog(`Debt position payment sending API invocation returned HTTP status code: ${response.status} with body: ${JSON. stringify(response.data)}`);
    assert.strictEqual(response.status, 200);

    /*
    response = await sendRT(buildSendRTRequest(bundle));
    debugLog(`Receipt sending API invocation returned HTTP status code: ${response.status} with body: ${JSON. stringify(response.data)}`);
    assert.match(response.data, /<outcome>OK<\/outcome>/);
    */
}

async function retrieveCreditorInstitution(bundle, organizationFiscalCode) {
    /* Read and save the creditor institution */
    let response = await readCreditorInstitution(organizationFiscalCode);
    debugLog(`Creditor institution with id [${organizationFiscalCode}] retrieving API invocation returned HTTP status code: ${response?.status} with body: ${JSON. stringify(response.data)}`);
    assert.strictEqual(response.status, 200);
    const creditorInstitution = response.data;    

    /* Read and save an IBAN for the creditor institution */
    response = await readCreditorInstitutionIbans(organizationFiscalCode);
    const ibanWrapper = response.data;
    debugLog(`Creditor institution ibans with id [${organizationFiscalCode}] retrieving API invocation returned HTTP status code: ${response?.status} with body: ${JSON. stringify(response.data)}`);
    assert.ok(ibanWrapper.ibans[0] !== undefined);
    bundle.creditorInstitution = {
        id: creditorInstitution.creditor_institution_code,
        businessName: creditorInstitution.business_name,
        enabled: creditorInstitution.enabled,
        iban: ibanWrapper.ibans[0].iban,
    }

    /* Returning generated object */
    return bundle.creditorInstitution;
}

async function retrieveBroker(bundle, brokerCode) {
    let response = await readCreditorInstitutionBroker(brokerCode);
    debugLog(`Creditor institution broker with id [${brokerCode}] retrieving API invocation returned HTTP status code: ${response?.status} with body: ${JSON. stringify(response.data)}`);
    assert.strictEqual(response.status, 200);
    const broker = response.data; 
    bundle.broker = {
        id: broker.broker_code,
        enabled: broker.enabled,
        description: broker.description,
    };

    /* Returning generated object */
    return bundle.broker;
}

async function retrieveStation(bundle, stationId) {
    /* Read and save the station */
    let response = await readStation(stationId);
    debugLog(`Station with id [${stationId}] retrieving API invocation returned HTTP status code: ${response?.status} with body: ${JSON. stringify(response.data)}`);
    assert.strictEqual(response.status, 200);
    const stationDetail = response.data;
    
    /* Read and save the station association with the creditor institution */
    response = await readCIStationAssociation(stationId, bundle.creditorInstitution.id);
    debugLog(`Station association with CI with id [${bundle.creditorInstitution.id}] retrieving API invocation returned HTTP status code: ${response?.status} with body: ${JSON. stringify(response.data)}`);
    assert.strictEqual(response.status, 200);
    const stationCIAssociation = response.data;
    bundle.station = {
        id: stationDetail.station_code,
        enabled: stationDetail.enabled,
        brokerCode: stationDetail.broker_code,
        ip: stationDetail.ip,
        port: stationDetail.port,
        password: stationDetail.password,
        auxDigit: stationCIAssociation.aux_digit,
        applicationCode: stationCIAssociation.application_code,
        segregationCode: stationCIAssociation.segregation_code,
    };

    /* Returning generated object */
    return bundle.station;
}

async function retrievePSP(bundle, pspId, channelId) {
    /* Read and save the PSP */
    let response = await readPSP(pspId);
    debugLog(`PSP with id [${pspId}] retrieving API invocation returned HTTP status code: ${response?.status} with body: ${JSON. stringify(response.data)}`);
    assert.strictEqual(response.status, 200);
    const pspDetail = response.data;

    /* Read and save the channel */
    response = await readChannel(channelId);
    debugLog(`Channel with id [${channelId}] retrieving API invocation returned HTTP status code: ${response?.status} with body: ${JSON. stringify(response.data)}`);
    assert.strictEqual(response.status, 200);
    const pspChannelAssociation = response.data;    
    bundle.psp = {
        id: pspDetail.psp_code,
        enabled: pspDetail.enabled,
        businessName: pspDetail.business_name,
        bic: pspDetail.bic,
        fiscalCode: pspDetail.tax_code,
        channelId: pspChannelAssociation.channel_code,
        channelPassword: pspChannelAssociation.password,
        brokerId: pspChannelAssociation.broker_psp_code,
    }

    /* Returning generated object */
    return bundle.psp;
}

async function generateDebtor(bundle) {
    const firstName = generateFirstName();
    const lastName = generateLastName();
    const city = generateCity();
    bundle.debtor = {
        fullName: `${firstName} ${lastName}`,
        fiscalCode: generateFiscalCode(firstName, lastName),
        streetName: generateStreet(),
        civicNumber: `${Math.floor(Math.random() * 1000) + 1}`,
        postalCode: city.postalCode,
        city: city.city,
        province: city.province,
        region: city.region,
        country: "IT",
        email: `${firstName.toLowerCase()}${lastName.toLowerCase()}@mail.com`,
        phone: `+39 3${generateRandomNumber(9)}`,
        companyName: generateCompany(),
        officeName: `Sede di ${city.city}`,
    };

    /* Returning generated object */
    debugLog(`Generated the debtor: ${JSON.stringify(bundle.debtor)}`);
    return bundle.debtor;
}

async function generateDebtPositionDetail(bundle) {
    const iuvPrefix = bundle.station.segregationCode < 10 ? `0${bundle.station.segregationCode}` : `${bundle.station.segregationCode}`;
    const debtPositionAmount = (Math.floor(Math.random() * 99) + 1) * 10;
    const debtor = bundle.debtor;
    const iuv = `${iuvPrefix}${makeidNumber(15)}`;
    const remainingDays = Math.floor(Math.random() * 29) + 1;
    const dueDate = addDays(remainingDays);
    const retentionDate = addDays(90 - remainingDays);
    bundle.debtPosition = {
        iupd: makeidMix(35),
        type: "F",
        fiscalCode: debtor.fiscalCode,
        fullName: debtor.fullName,
        streetName: debtor.streetName,
        civicNumber: debtor.civicNumber,
        postalCode: debtor.postalCode,
        city: debtor.city,
        province: debtor.province,
        region: debtor.region,
        country: debtor.country,
        email: debtor.email,
        phone: debtor.phone,
        companyName: debtor.companyName,
        officeName: debtor.officeName,
        paymentOption: [
            {
                iuv: iuv,
                amount: debtPositionAmount * 100,
                description: `Conguaglio CUP - ${bundle.debtor.companyName}`,
                isPartialPayment: false,
                dueDate: dueDate,
                retentionDate: retentionDate,
                fee: 0,
                transfer: [
                    {
                        idTransfer: '1',
                        amount: (debtPositionAmount * 100 / 5) * 2,
                        remittanceInformation: "Prima Rata",
                        category: "9/0101108TS/",
                        iban: bundle.creditorInstitution.iban,
                    },
                    {
                        idTransfer: '2',
                        amount: (debtPositionAmount * 100 / 5) * 3,
                        remittanceInformation: "Seconda Rata",
                        category: "9/0101108TS/",
                        iban: bundle.creditorInstitution.iban,
                    }
                ]
            }
        ]
    };
    bundle.debtPositionCalculation = {
        decimalAmount: `${bundle.debtPosition.paymentOption[0].amount / 100}.00`,
        idempotencyKey: `${iuv.substring(6, 17)}_${makeidNumber(6)}${makeidMix(4).toUpperCase()}`,
        noticeNumber: `3${iuv}`,
        applicationDate: buildStringFromDate(addDays(0)),
        transferDate: buildStringFromDate(addDays(1)),
        receiptId: makeidMix(33),
    }

    /* Returning generated object */
    debugLog(`Generated the debt position detail: ${JSON.stringify(bundle.debtPosition)}`);
    debugLog(`Generated the debt position calculation detail: ${JSON.stringify(bundle.debtPositionCalculation)}`);
    return bundle.debtPosition;
}

module.exports = {
    generateAndPayDebtPosition,
    generateDebtPosition,
    retrievePaymentOptionDetail,
}