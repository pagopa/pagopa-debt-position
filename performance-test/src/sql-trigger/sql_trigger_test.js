const { insertPaymentPositionWithValidFiscalCode, updatePaymentPositionStatus, insertPaymentOption, deletePaymentPositions,deletePaymentOptions } = require("./modules/pg_gpd_client.js");
const {shutDownPool} = require("./modules/pg_gpd_client");

const NUMBER_OF_RECORDS =JSON.parse(process.env.NUMBER_OF_RECORDS);

async function insertEvents() {
    console.log("Cleaning up before execution...");
    await cleanup();

    const startTime = Date.now();  // process.hrtime() for higher precision.

    //const arrayIdTokenized = [];

    console.log("Selected number of events: ", NUMBER_OF_RECORDS);
    // POPULATE ON DB paymentPositions
    for (let i = 0; i < NUMBER_OF_RECORDS ; i++) {
        const uniqueId = 220798 + i;

        await insertPaymentPositionWithValidFiscalCode(uniqueId);
        //arrayIdTokenized.push(idValidFiscalCode);

        await insertPaymentOption(uniqueId);

    }

    // console.log(`Inserted ${arrayIdTokenized.length} elements in database paymentPositions with valid fiscal code with ids: `, JSON.stringify(arrayIdTokenized));
    // console.log(`Inserted ${arrayIdTokenized.length} elements in database paymentOptions with same ids `); //: ,JSON.stringify(arrayIdTokenized));

    console.log(`Inserted ${NUMBER_OF_RECORDS} elements in database paymentPositions with valid fiscal code`);
    console.log(`Inserted ${NUMBER_OF_RECORDS} elements in database paymentOptions `);


    // UPDATE paymentPositions
    console.log(`Updating status on ${NUMBER_OF_RECORDS} elements in database paymentPositions `);
    await updatePaymentPositionStatus();
    console.log(`Updated status on ${NUMBER_OF_RECORDS} elements in database paymentPositions `);

    const endTime = Date.now();
    console.log(`Execution time: ${(endTime - startTime) / 1000} seconds`);

    console.log("Cleaning up...");
    await cleanup();
    await shutDownPool();

    return null;
}

async function cleanup() {

    await deletePaymentOptions();
    console.log("Deleted payment options");

    await deletePaymentPositions();
    console.log("Deleted payment positions");
}

insertEvents().then(() => {
    console.log("Trigger performance test script ended");
    process.exit();
});