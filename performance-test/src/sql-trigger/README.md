# SQL Trigger Performance Test

This script (`sql_trigger_test.js`) allows to manually test the performance with SQL triggers enabled or disabled for the `paymentPositions` and `paymentOptions` tables.

## Prerequisites

- Node.js installed
- Environment variables for database connection configured
- `NUMBER_OF_RECORDS` environment variable set with the number of records to insert

## Installation

1. Install dependencies:
   ```bash
   npm install
   ```

## Usage

With SQL triggers enabled or disabled, you can run the performance test on different environments using the scripts provided in the `package.json`:

The script performs the following operations:
- Cleans the tables (`paymentPositions`, `paymentOptions`) from any test data specific to this test, if present
- Inserts the specified number of records
- Updates the `status` field of the inserted records in `paymentPositions`
- Measures and prints the execution time
- Cleans the tables (`paymentPositions`, `paymentOptions`) again from any test data specific to this test

## Output

During execution, the script prints:
- Number of records inserted
- Total execution time
- Status of cleanup operations
- Any errors encountered
