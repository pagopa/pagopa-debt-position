const { addDays, buildStringFromDate, makeidNumber, makeidMix,  } = require("./helpers");

function buildDebtPositionDynamicData(gpdSessionBundle, iupdIn) {
    return {
        iupd: iupdIn,
        iuv1: "IUV" + makeidNumber(14),
        iuv2: "IUV" + makeidNumber(14),
        iuv3: "IUV" + makeidNumber(14),
        iuvOK: process.env.iuv_ok,  // es. "11101751670642134"
        iuvKO: process.env.iuv_ko,  // es. "03163674189686371"
        iban: gpdSessionBundle.debtPosition.iban,
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

function buildCreateDebtPositionRequest(debtPosition, payer) {
    return {
        iupd: debtPosition.iupd,
        type: "F",
        fiscalCode: payer.fiscalCode,
        fullName: payer.name,
        streetName: payer.streetName,
        civicNumber: payer.civicNumber,
        postalCode: payer.postalCode,
        city: payer.city,
        province: payer.province,
        region: payer.region,
        country: payer.country,
        email: payer.email,
        phone: payer.phone,
        companyName: payer.companyName,
        officeName: payer.officeName,
        switchToExpired: false,
        paymentOption: [
            {
                iuv: debtPosition.iuv1,
                amount: debtPosition.amount * 100,
                description: "Canone Unico Patrimoniale - SkyLab Inc.",
                isPartialPayment: false,
                dueDate: debtPosition.dueDate,
                retentionDate: debtPosition.retentionDate,
                fee: 0,
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
            }
        ]
    };
}

function buildCreateOK_KODebtPositionRequest(bundle, action) {
	let debtPosition = bundle.debtPosition;
	let payer = bundle.payer;
    return {
        iupd: debtPosition.iupd,
        type: "F",
        fiscalCode: payer.fiscalCode,
        fullName: payer.name,
        streetName: payer.streetName,
        civicNumber: payer.civicNumber,
        postalCode: payer.postalCode,
        city: payer.city,
        province: payer.province,
        region: payer.region,
        country: payer.country,
        email: payer.email,
        phone: payer.phone,
        companyName: payer.companyName,
        officeName: payer.officeName,
        switchToExpired: false,
        paymentOption: [
            {
                iuv: action == "OK" ? debtPosition.iuvOK:debtPosition.iuvKO,
                amount: debtPosition.amount * 100,
                description: "Canone Unico Patrimoniale - SkyLab Inc.",
                isPartialPayment: false,
                dueDate: debtPosition.dueDate,
                retentionDate: debtPosition.retentionDate,
                fee: 0,
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
                        organizationFiscalCode: bundle.organizationCode,
                        amount: (debtPosition.amount * 100 / 3) * 2,
                        remittanceInformation: "Rata 2",
                        category: "9/0101108TS/",
                        iban: debtPosition.iban,
                    }
                ]
            }
        ]
    };
}

function buildUpdateDebtPositionRequest(debtPosition, payer) {
    return {
        iupd: debtPosition.iupd,
        type: "F",
        fiscalCode: payer.fiscalCode,
        fullName: payer.name,
        streetName: payer.streetName,
        civicNumber: payer.civicNumber,
        postalCode: payer.postalCode,
        city: payer.city,
        province: payer.province,
        region: payer.region,
        country: payer.country,
        email: payer.email,
        phone: payer.phone,
        companyName: payer.companyName + " - Edit",
        officeName: payer.officeName + " - Edit",
        switchToExpired: false,
        paymentOption: [
            {
                iuv: debtPosition.iuv1,
                amount: debtPosition.amount * 100,
                description: "Canone Unico Patrimoniale - SkyLab Inc. - Edit",
                isPartialPayment: false,
                dueDate: debtPosition.dueDate,
                retentionDate: debtPosition.retentionDate,
                fee: 0,
                transfer: [
                    {
                        idTransfer: debtPosition.transferId1,
                        amount: (debtPosition.amount * 100 / 3),
                        remittanceInformation: "Rata 1 Edit",
                        category: "9/0101108TS/",
                        iban: debtPosition.iban,
                    },
                    {
                        idTransfer: debtPosition.transferId2,
                        organizationFiscalCode: debtPosition.transferOtherCIFiscalCode,
                        amount: (debtPosition.amount * 100 / 3) * 2,
                        remittanceInformation: "Rata 2 Edit",
                        category: "9/0101108TS/",
                        iban: debtPosition.iban,
                    }
                ]
            }
        ]
    };
}

function buildUpdateDebtPositionInfoRequest(debtPosition, payer) {
    return {
        iupd: debtPosition.iupd,
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
        companyName: payer.companyName,
        officeName: payer.officeName,
        switchToExpired: false,
        paymentOption: [
            {
                iuv: debtPosition.paymentOption[0].iuv,
                amount: debtPosition.paymentOption[0].amount * 100,
                description: "Canone Unico Patrimoniale - SkyLab Inc. - Edit",
                isPartialPayment: false,
                dueDate: debtPosition.paymentOption[0].dueDate,
                retentionDate: debtPosition.paymentOption[0].retentionDate,
                fee: 0,
                transfer: [
                    {
                        idTransfer: debtPosition.paymentOption[0].transfer[0].idTransfer,
                        amount: (debtPosition.paymentOption[0].amount * 100 / 3),
                        remittanceInformation: "Rata 1 Edit",
                        category: "9/0101108TS/",
                        iban: debtPosition.paymentOption[0].transfer[0].iban,
                    },
                    {
                        idTransfer: debtPosition.paymentOption[0].transfer[1].idTransfer,
                        organizationFiscalCode: debtPosition.transferOtherCIFiscalCode,
                        amount: (debtPosition.paymentOption[0].amount * 100 / 3) * 2,
                        remittanceInformation: "Rata 2 Edit",
                        category: "9/0101108TS/",
                        iban: debtPosition.paymentOption[0].transfer[1].iban,
                    }
                ]
            }
        ]
    };
}

module.exports = {
    buildCreateDebtPositionRequest,
    buildUpdateDebtPositionRequest,
    buildDebtPositionDynamicData,
    buildUpdateDebtPositionInfoRequest,
    buildCreateOK_KODebtPositionRequest
}