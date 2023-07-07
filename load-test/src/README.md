# Load tests for GPD project

- [Load tests for GPD project](#load-tests-for-gpd-project)
  - [01. Debtor position creation](#01-debtor-position-creation)
  - [02. Payment workflow](#02-payment-workflow)

This is a set of [k6](https://k6.io) load tests related to the GPD (_Gestione Posizioni Debitorie_) initiative.

To invoke k6 load test passing parameter use -e (or --env) flag:

```
-e MY_VARIABLE=MY_VALUE
# example
-e API_SUBSCRIPTION_KEY=<your-secret>
```

## How to run 🚀

Use this command to launch the load tests:

```
k6 run -e VARS=./environments/dev.environment.json -e TEST_TYPE=./test-types/<type>.json <script-name>.js
```
or
```
k6 run --e TEST_TYPE=./test-types/load.json -e BASE_URL=<protocol>://<host>:<port> load-test/src/payments_workflow.js
```

### 01. Debtor position creation

Call to test the creation of a debt position:

#### Azure DEV environment
```
k6 run -e TEST_TYPE=./test-types/load.json -e BASE_URL=https://api.dev.platform.pagopa.it/gpd/api/v1 gpd/load-test/src/create_debt_postion.js
```

#### Azure UAT environment
```
k6 run -e TEST_TYPE=./test-types/load.json -e BASE_URL=https://api.uat.platform.pagopa.it/gpd/api/v1 gpd/load-test/src/create_debt_postion.js
```

#### Local host
```
k6 run --e TEST_TYPE=./test-types/load.json -e BASE_URL=http://localhost:8080 gpd/load-test/src/create_debt_postion.js
```


### 02. Payment workflow

Call to test the payment workflow:
1. create debt position without validity date
2. publish the debt position
3. pay a payment option
4. reports a transaction
5. get the details for the paid payment option

#### Azure DEV environment
```
k6 run --e TEST_TYPE=./test-types/load.json -e BASE_URL=https://api.dev.platform.pagopa.it/gpd/api/v1 gpd/load-test/src/create_debt_postion.js
```

#### Azure UAT environment
```
k6 run --e TEST_TYPE=./test-types/load.json -e BASE_URL=https://api.uat.platform.pagopa.it/gpd/api/v1 gpd/load-test/src/payments_workflow.js
```

#### Local host
```
k6 run --e TEST_TYPE=./test-types/load.json -e BASE_URL=http://localhost:8080 gpd/load-test/src/payments_workflow.js
```