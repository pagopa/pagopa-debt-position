# Load tests for Reporting project


- [Load tests for Reporting project](#load-tests-for-reporting-project)
  - [01. get flow list call](#01-get-flow-list)
  - [02. get flow call](#02-get-flow)

This is a set of [k6](https://k6.io) load tests related to the GPD (_Gestione Posizioni Debitorie_) initiative.

To invoke k6 load test passing parameter use -e (or --env) flag:

```
-e MY_VARIABLE=MY_VALUE
```

## Prerequisites

- [Azurite open-source emulator](https://docs.microsoft.com/en-us/azure/storage/common/storage-use-azurite?tabs=docker-hub) is required for local testing

## 01. Get Flow List

Call to test the get flow list call:
1. initializes the table by inserting test records
2. execute rest call
3. clears the table from the test records entered

Example command launch on localhost 

```
k6 run -e BASE_URL=http://localhost:7071 -e FLOW_TABLE_URL=http://127.0.0.1:10002/devstoreaccount1/flows -e ACCESS_SIGNATURE_QUERY_STRING="?sv=2018-03-28&se=2022-04-28T12%3A15%3A36Z&sp=raud&sig=dwGaSA6cXmAk6ZHPrhrg97up9qmvm0W6IAFUAYfKTdA%3D&tn=flows" -e NUM_OF_ORGANIZATIONS=5 -e NUM_FLOW_ID_FOR_ORG=10 reporting-analysis\load-test\src\reporting-analysis_get_flow_list.js
```

To obtain ACCESS_SIGNATURE_QUERY_STRING from the app 'Microsoft Azure Storage Explorer':
- right click on table
- Get Shared Access Signature
- Select all auth checkbox
- click create

To generate a  shared access signature (SAS) without downloading the 'Microsoft Azure Storage Explorer' app please refer to the following [link](https://docs.microsoft.com/en-us/rest/api/storageservices/create-account-sas)

The NUM_OF_ORGANIZATIONS variable represents the number of fake organizations to insert for the test

The NUM_FLOW_ID_FOR_ORG variable represents the number of fake flow id for each organization to insert for the test

## 02. Get Flow

Call to test the get flow list call:
1. initializes the blob container by inserting test files
2. execute rest call
3. clears the blob container from the test files

Example command launch on localhost 

```
k6 run -e BASE_URL=http://localhost:7071 -e FLOW_BLOB_URL=http://127.0.0.1:10000/devstoreaccount1/blob -e ACCESS_SIGNATURE_QUERY_STRING="?sv=2018-03-28&st=2022-04-29T07%3A00%3A00Z&se=2022-04-30T06%3A57%3A39Z&sr=c&sp=racwdl&sig=KqE4MhZOOE4N%2BBOrm6UKVACno6LaBkPE36fIthj3Pn8%3D" -e FILENAME=simpleXML.xml -e NUM_OF_ORGANIZATIONS=5 -e NUM_FLOW_ID_FOR_ORG=10 reporting-analysis\load-test\src\reporting-analysis_get_flow.js
```

To obtain ACCESS_SIGNATURE_QUERY_STRING from Microsoft Azure Storage Explorer:
- right click on table
- Get Shared Access Signature
- Select the auth checkbox: Read, Add, Create, Write, Delete, List
- click create

To generate a  shared access signature (SAS) without downloading the 'Microsoft Azure Storage Explorer' app please refer to the following [link](https://docs.microsoft.com/en-us/rest/api/storageservices/create-account-sas)

The NUM_OF_ORGANIZATIONS variable represents the number of fake organizations to generate for the test

The NUM_FLOW_ID_FOR_ORG variable represents the number of fake flow id for each organization to generate for the test
 