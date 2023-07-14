# Integration Tests
👀 Integration tests are in `integration-test/src/` folder. See there for more information. 

## How run in local environment

To run the integration test from local environment, simply execute this commands:
```
export API_CONFIG_SUBSCRIPTION_KEY=##SUBSCRIPTION_KEY## \
export GPD_SUBSCRIPTION_KEY=##SUBSCRIPTION_KEY## \
export PAYMENTS_REST_SUBSCRIPTION_KEY=##SUBSCRIPTION_KEY## \
export PAYMENTS_SOAP_SUBSCRIPTION_KEY=##SUBSCRIPTION_KEY## \
export REPORTING_SUBSCRIPTION_KEY=##SUBSCRIPTION_KEY## \
export REPORTING_BATCH_CONNECTION_STRING=##QUEUE_CONNECTION_STRING## \    
yarn install && \
yarn test:${{ parameters.ENV }}
```

## How run on Docker 🐳

To run the integration tests on docker, you can run from this directory the script:


``` shell
sh ./run_integration_test.sh <local|dev|uat|prod> <api-subscription-key> <apiconfig_subkey> <gpd_subkey> <gps_spontaneouspayments_subkey> <gps_donations_subkey> <payments_subkey>
```


ℹ️ _Note_: for **PagoPa ACR** is **required** the login `az acr login -n <acr-name>`

If you use dev, uat or prod **you test the images on Azure ACR**

---
💻 If you want to test your local branch,
``` shell
sh ./run_integration_test.sh local <api-subscription-key> <apiconfig_subkey> <gpd_subkey> <gps_spontaneouspayments_subkey> <gps_donations_subkey> <payments_subkey>
```

