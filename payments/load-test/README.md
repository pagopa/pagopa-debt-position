# Load tests for Payments project

- [Load tests for Payments project](#load-tests-for-payments-project)
  - [01. Payment workflow](#01-payment-workflow)

This is a set of [k6](https://k6.io) load tests related to the GPD (_Gestione Posizioni Debitorie_) initiative.

To invoke k6 load test passing parameter use -e (or --env) flag:

```
-e MY_VARIABLE=MY_VALUE
```

## 01. Payment workflow

Call to test the payment workflow:
1. create debt position without validity date
2. publish the debt position
3. verify payment
4. activate payment
5. send receipt

Example command launch on Azure

```
k6 run -e BASE_GPD_URL=https://api.dev.platform.pagopa.it/gpd/api/v1 -e BASE_PAYMENTS_URL=https://api.dev.platform.pagopa.it/payments/api/v1 -e ID_BROKER_PA=15376371009 -e ID_STATION=15376371009_01 payments/load-test/src/payments_workflow.js

k6 run -e BASE_GPD_URL=https://api.uat.platform.pagopa.it/gpd/api/v1 -e BASE_PAYMENTS_URL=https://api.uat.platform.pagopa.it/payments/api/v1 -e ID_BROKER_PA=15376371009 -e ID_STATION=15376371009_01 payments/load-test/src/payments_workflow.js
```

Example command launch on localhost 

```
k6 run -e BASE_GPD_URL=http://localhost:8085 -e BASE_PAYMENTS_URL=http://localhost:8080 -e ID_BROKER_PA=77777777777 -e ID_STATION=77777777777_1 payments/load-test/src/payments_workflow.js
```