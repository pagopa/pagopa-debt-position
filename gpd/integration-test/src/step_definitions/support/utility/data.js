let gpdSessionBundle = {
    isExecuting: false,
    responseToCheck: undefined,
    organizationCode: undefined,
    brokerCode: undefined,
    stationCode: undefined,
    debtPosition: {
        iupd: undefined,
        iuv1: undefined,
        iuv2: undefined,
        iuv3: undefined,
        iuvPrefix: 'IUV',
        iban: 'mockIban',
        dueDate: undefined,
        retentionDate: undefined,
        transferId1: undefined,
        transferId2: undefined,
        receiptId: undefined,
        amount: undefined,
        pspId: undefined,
        pspBrokerId: undefined,
        pspChannelId: undefined,
        pspName: undefined,
        pspFiscalCode: undefined,
        fiscalCode: undefined,
        paymentToken: undefined,
        applicationDate: undefined,
        transferDate: 'undefined',
    },
    payer: {
        name: "Michele Ventimiglia",
        fiscalCode: "VNTMHL76M09H501D",
        streetName: "via Washington",
        civicNumber: "11",
        postalCode: "89812",
        city: "Pizzo Calabro",
        province: "VV",
        region: "CA",
        country: "IT",
        email: "micheleventimiglia@skilabmail.com",
        phone: "333-123456789",
        companyName: "SkyLab Inc.",
        officeName: "SkyLab - Sede via Washington"
    },
}

module.exports = {
    gpdSessionBundle
}