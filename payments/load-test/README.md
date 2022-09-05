# Load tests for Payments project

- [Load tests for Payments project](#load-tests-for-payments-project)
  - [01. Payment workflow](#01-payment-workflow)
  - [02. CU Scenario Payment workflow](#02-cu-scenario-payment-workflow)
  - [03. DemandNotice](#03-demandnotice)

This is a set of [k6](https://k6.io) load tests related to the GPD (_Gestione Posizioni Debitorie_) initiative.

To invoke k6 load test passing parameter use -e (or --env) flag:

```
-e MY_VARIABLE=MY_VALUE
```

## 01. Payment workflow

Call to test the payment workflow:
On Gpd
1. create debt position without validity date
2. publish the debt position
On Payments
1. VerifyPayment
2. ActivatePayment
3. SendRT

For Azure

_Payment workflow_
```
k6 run -e BASE_GPD_URL=https://api.dev.platform.pagopa.it/gpd/api/v1 -e BASE_PAYMENTS_URL=https://api.dev.platform.pagopa.it/gpd-payments/api/v1 -e ID_BROKER_PA=15376371009 -e ID_STATION=15376371009_01 payments/load-test/src/payments_workflow.js
```


## 02. CU Scenario Payment workflow

Call to test the payment workflow: 
> Given a CSV input file (see `example.csv` file)

On Payments
1. VerifyPayment
2. ActivatePayment
3. SendRT

For Azure   

_CU scenario : Payment workflow_ given a specific input csv
```
k6 run -e FILENAME=example.csv -e BASE_PAYMENTS_URL=https://api.dev.platform.pagopa.it/gpd-payments/api/v1 -e ID_BROKER_PA=15376371009 -e ID_STATION=15376371009_01 payments/load-test/src/payments_workflow.oneshot.js
```


## 03. DemandNotice

To run the test:

```
k6 run --env VARS=dev.environment.json --env TEST_TYPE=./test-types/load.json create_spontaneous_payment.js
```

1. See `./test-type` folder for `TEST_TYPE` values
2. See `dev.environment.json` file to set your environment
