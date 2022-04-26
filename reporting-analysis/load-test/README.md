# Load tests for Payments project


- [Load tests for Payments project](#load-tests-for-payments-project)
  - [01. get flow list call](#01-get-flow-list)

This is a set of [k6](https://k6.io) load tests related to the GPD (_Gestione Posizioni Debitorie_) initiative.

To invoke k6 load test passing parameter use -e (or --env) flag:

```
-e MY_VARIABLE=MY_VALUE
```

## 01. Get Flow List

Call to test the get flow list call:
1. initializes the table by inserting test records
2. execute rest call
3. clears the table from the test records entered

Example command launch on localhost 

```
k6 run -e BASE_URL=http://localhost:7071 -e FLOW_TABLE_URL=http://127.0.0.1:10002/devstoreaccount1/flows -e ACCESS_SIGNATURE_QUERY_STRING="?sv=2018-03-28&se=2022-04-28T12%3A15%3A36Z&sp=raud&sig=dwGaSA6cXmAk6ZHPrhrg97up9qmvm0W6IAFUAYfKTdA%3D&tn=flows" -e NUM_OF_ORGANIZATIONS=5 -e NUM_FLOW_ID_FOR_ORG=10 C:\Users\aacitelli\git\PAGOPA\pagopa-debt-position\reporting-analysis\load-test\src\reporting-analysis_get_flow_list.js
```

To obtain ACCESS_SIGNATURE_QUERY_STRING from Microsoft Azure Storage Explorer:
- right click on table
- Get Shared Access Signature
- Select all auth checkbox
- click create

The NUM_OF_ORGANIZATIONS variable represents the number of fake organizations to insert for the test

The NUM_FLOW_ID_FOR_ORG variable represents the number of fake flow id for each organization to insert for the test

 