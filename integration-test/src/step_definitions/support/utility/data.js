let bundle = {
    creditorInstitution: {
        id: undefined,
        enabled: false,
        businessName: undefined,
        iban: undefined,
    },
    broker: {
        id: undefined,
        enabled: false,
        description: undefined,
    },
    station: {
        id: undefined,
        enabled: false,
        brokerCode: undefined,
        ip: undefined,
        port: undefined,
        password: undefined,
        auxDigit: undefined,
        applicationCode: undefined,
        segregationCode: undefined,
    },
    psp: {
        id: undefined,
        enabled: undefined,
        businessName: undefined,
        bic: undefined,
        fiscalCode: undefined,
        channelId: undefined,
        channelPassword: undefined,
        brokerId: undefined,
    },
    debtor: {
        fullName: undefined,
        fiscalCode: undefined,
        streetName: undefined,
        civicNumber: undefined,
        postalCode: undefined,
        city: undefined,
        province: undefined,
        region: undefined,
        country: undefined,
        email: undefined,
        phone: undefined,
        companyName: undefined,
        officeName: undefined,
    },
    debtPosition: {
        iupd: undefined,
        type: undefined,
        fiscalCode: undefined,
        paymentOption: [
            {
                iuv: undefined,
                amount: undefined,
                description: undefined,
                isPartialPayment: false,
                dueDate: undefined,
                retentionDate: undefined,
                fee: undefined,
                transfer: [
                    {
                        idTransfer: undefined,
                        amount: undefined,
                        remittanceInformation: undefined,
                        category: undefined,
                        iban: undefined,
                    },
                    {
                        idTransfer: undefined,
                        amount: undefined,
                        remittanceInformation: undefined,
                        category: undefined,
                        iban: undefined,
                    },
                ]
            }
        ]
    },
    debtPositionCalculation: {
        decimalAmount: undefined,
        idempotencyKey: undefined,
        noticeNumber: undefined,
        applicationDate: undefined,
        transferDate: undefined,
        paymentToken: undefined,
        receiptId: undefined,
    },
    flow: {
        id: undefined,
        date: undefined,
        dateAndTime: undefined,
    },
    response: undefined
};

module.exports = {
    bundle
}