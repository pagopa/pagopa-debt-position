const { addDays, buildStringFromDate, makeidNumber, makeidMix,  } = require("./helpers");

function buildDebtPositionDynamicData(gpdSessionBundle, iupdIn) {
    return {
        iupd: iupdIn,
        iuv1: makeidNumber(17),
        iuv2: makeidNumber(17),
        iuv3: makeidNumber(17),
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

module.exports = {
    buildCreateDebtPositionRequest,
    buildDebtPositionDynamicData
}