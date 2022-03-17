# Load tests for GPD project

- [01. Debtor position creation](#01-debtor-position-creation)
- [02. Payment workflow](#02-payment-workflow)

This is a set of [k6](https://k6.io) load tests related to the GPD (_Gestione Posizioni Debitorie_) initiative.

To invoke k6 load test passing parameter use -e (or --env) flag:

```
-e MY_VARIABLE=MY_VALUE
```

## 01. Debtor position creation

Call to test the creation of a debt position:

```
k6 run gpd/load-test/src/create_debt_postion.js
```
## 02. Payment workflow

Call to test the payment workflow:
1. create debt position without validity date
2. publish the debt position
3. pay a payment option
4. reports a transaction
5. get the details for the paid payment option

```
k6 run -e BASE_URL=<protocol>://<host>:<port> gpd/load-test/src/payments_workflow.js
```