const { addDays, buildStringFromDate, makeidNumber, makeidMix, } = require("./helpers");

function buildDebtPositionDynamicDataV3(gpdSessionBundle, iupdIn, iuv = makeidNumber(17), validityDate = null) {
    return {
        iupd: iupdIn,
        iuv1: iuv,
        iuv2: makeidNumber(17),
        iuv3: makeidNumber(17),
        iuvOK: process.env.iuv_ok,  // es. "11101751670642134"
        iuvKO: process.env.iuv_ko,  // es. "03163674189686371"
        iban: gpdSessionBundle.debtPosition.iban,
        validityDate: validityDate,
        dueDate: addDays(30),
        retentionDate: addDays(90),
        transferId1: '1',
        transferId2: '2',
        amount: 300.00,
        receiptId: makeidMix(33),
        pspId: "60000000001",
        pspBrokerId: "60000000001",
        pspChannelId: "60000000001_01",
        pspName: "PSP Paolo",
        pspFiscalCode: "CF60000000006",
        idempotency: `60000000001_${makeidNumber(6)}${makeidMix(4)}`,
        applicationDate: buildStringFromDate(addDays(0)),
        transferDate: buildStringFromDate(addDays(1)),
        transferOtherCIFiscalCode: "01020304059"
    };
}

/**
 * Build the request of PaymentPositionModelV3 model
 */
function buildCreateDebtPositionRequestV3(debtPosition, payer) {
    return {
        iupd: debtPosition.iupd,
        companyName: payer.companyName ? payer.companyName : "SkyLab Inc.",
        officeName: payer.officeName,
        paymentOption: [
            {
                validityDate: debtPosition.validityDate,
                retentionDate: debtPosition.retentionDate,
                switchToExpired: false,
                debtor: {
                    type: "F",
                    fiscalCode: payer.fiscalCode,
                    fullName: payer.fullName,
                    streetName: payer.streetName,
                    civicNumber: payer.civicNumber,
                    postalCode: payer.postalCode,
                    city: payer.city,
                    province: payer.province,
                    region: payer.region,
                    country: payer.country,
                    email: payer.email,
                    phone: payer.phone,
                },
                installments: [
                    {
                        iuv: debtPosition.iuv1,
                        amount: debtPosition.amount * 100,
                        description: "Canone Unico Patrimoniale - SkyLab Inc.",
                        dueDate: debtPosition.dueDate,
                        transfer: [
                            {
                                idTransfer: debtPosition.transferId1,
                                amount: (debtPosition.amount * 100 / 3),
                                remittanceInformation: "Rata 1",
                                category: "9/0101108TS/",
                                iban: debtPosition.iban,
                            },
                            {
                                idTransfer: debtPosition.transferId2,
                                organizationFiscalCode: debtPosition.transferOtherCIFiscalCode,
                                amount: (debtPosition.amount * 100 / 3) * 2,
                                remittanceInformation: "Rata 2",
                                category: "9/0101108TS/",
                                iban: debtPosition.iban,
                            }
                        ]
                        // installmentMetadata: [] // Optional
                    }
                ]
            }
        ]
    };
}

/**
 * Build the request OK/KO (PaymentPositionModelV3)
 */
function buildCreateOK_KODebtPositionRequestV3(bundle, action) {
    let debtPosition = bundle.debtPosition;
    let payer = bundle.payer;
    return {
        iupd: debtPosition.iupd,
        companyName: payer.companyName ? payer.companyName : "SkyLab Inc.",
        officeName: payer.officeName,
        paymentOption: [
            {
                validityDate: debtPosition.validityDate,
                retentionDate: debtPosition.retentionDate,
                switchToExpired: false,
                debtor: {
                    type: "F",
                    fiscalCode: payer.fiscalCode,
                    fullName: payer.fullName,
                    streetName: payer.streetName,
                    civicNumber: payer.civicNumber,
                    postalCode: payer.postalCode,
                    city: payer.city,
                    province: payer.province,
                    region: payer.region,
                    country: payer.country,
                    email: payer.email,
                    phone: payer.phone,
                },
                installments: [
                    {
                        iuv: action == "OK" ? debtPosition.iuvOK : debtPosition.iuvKO,
                        amount: debtPosition.amount * 100,
                        description: "Canone Unico Patrimoniale - SkyLab Inc.",
                        dueDate: debtPosition.dueDate,
                        transfer: [
                            {
                                idTransfer: debtPosition.transferId1,
                                amount: (debtPosition.amount * 100 / 3),
                                remittanceInformation: "Rata 1",
                                category: "9/0101108TS/",
                                iban: debtPosition.iban,
                            },
                            {
                                idTransfer: debtPosition.transferId2,
                                organizationFiscalCode: bundle.organizationCode, // Logica preservata
                                amount: (debtPosition.amount * 100 / 3) * 2,
                                remittanceInformation: "Rata 2",
                                category: "9/0101108TS/",
                                iban: debtPosition.iban,
                            }
                        ]
                    }
                ]
            }
        ]
    };
}

/**
 * Build the Update request (PaymentPositionModelV3)
 */
function buildUpdateDebtPositionRequestV3(debtPosition, payer) {
    return {
        iupd: debtPosition.iupd,
        companyName: payer.companyName ? payer.companyName : "SkyLab Inc. - Edit",
        officeName: payer.officeName + " - Edit",
        paymentOption: [
            {
                validityDate: debtPosition.validityDate,
                retentionDate: debtPosition.retentionDate,
                switchToExpired: false,
                debtor: {
                    type: "F",
                    fiscalCode: payer.fiscalCode,
                    fullName: payer.fullName,
                    streetName: payer.streetName,
                    civicNumber: payer.civicNumber,
                    postalCode: payer.postalCode,
                    city: payer.city,
                    province: payer.province,
                    region: payer.region,
                    country: payer.country,
                    email: payer.email,
                    phone: payer.phone,
                },
                installments: [
                    {
                        iuv: debtPosition.iuv1,
                        amount: debtPosition.amount * 100,
                        description: "Canone Unico Patrimoniale - SkyLab Inc. - Edit", // Edit in installment
                        isPartialPayment: false,
                        dueDate: debtPosition.dueDate,
                        transfer: [
                            {
                                idTransfer: debtPosition.transferId1,
                                amount: (debtPosition.amount * 100 / 3),
                                remittanceInformation: "Rata 1 Edit", // Edit in transfer
                                category: "9/0101108TS/",
                                iban: debtPosition.iban,
                            },
                            {
                                idTransfer: debtPosition.transferId2,
                                organizationFiscalCode: debtPosition.transferOtherCIFiscalCode,
                                amount: (debtPosition.amount * 100 / 3) * 2,
                                remittanceInformation: "Rata 2 Edit", // Edit in transfer
                                category: "9/0101108TS/",
                                iban: debtPosition.iban,
                            }
                        ]
                    }
                ]
            }
        ]
    };
}

/**
 * Build the Update request (PaymentPositionModelV3)
 */
function buildUpdateDebtPositionInfoRequestV3(debtPosition, payer) {
    const paymentOption = debtPosition.paymentOption[0];
    const installment = paymentOption.installments[0];
    const transfer0 = installment.transfer[0];
    const transfer1 = installment.transfer[1];

    return {
        iupd: debtPosition.iupd,
        companyName: payer.companyName ? payer.companyName : "SkyLab Inc.",
        officeName: payer.officeName,
        paymentOption: [
            {
                validityDate: paymentOption.validityDate,
                retentionDate: paymentOption.retentionDate,
                switchToExpired: false,
                debtor: {
                    type: "F",
                    fiscalCode: payer.fiscalCode,
                    fullName: payer.fullName,
                    streetName: payer.streetName,
                    civicNumber: payer.civicNumber,
                    postalCode: payer.postalCode,
                    city: payer.city,
                    province: payer.province,
                    region: payer.region,
                    country: payer.country,
                    email: payer.email,
                    phone: payer.phone,
                },
                installments: [
                    {
                        iuv: installment.iuv,
                        amount: installment.amount,
                        description: "Canone Unico Patrimoniale - SkyLab Inc. - Edit",
                        dueDate: installment.dueDate,
                        fee: 0,
                        transfer: [
                            {
                                idTransfer: transfer0.idTransfer,
                                // Ricalcolo 1/3
                                amount: Math.floor(installment.amount / 3),
                                remittanceInformation: "Rata 1 Edit",
                                category: "9/0101108TS/",
                                iban: transfer0.iban,
                            },
                            {
                                idTransfer: transfer1.idTransfer,
                                organizationFiscalCode: transfer1.organizationFiscalCode,
                                // Ricalcolo 2/3
                                amount: installment.amount - Math.floor(installment.amount / 3),
                                remittanceInformation: "Rata 2 Edit",
                                category: "9/0101108TS/",
                                iban: transfer1.iban,
                            }
                        ]
                    }
                ]
            }
        ]
    };
}


module.exports = {
    buildCreateDebtPositionRequestV3,
    buildUpdateDebtPositionRequestV3,
    buildDebtPositionDynamicDataV3,
    buildCreateOK_KODebtPositionRequestV3,
    buildUpdateDebtPositionInfoRequestV3
}