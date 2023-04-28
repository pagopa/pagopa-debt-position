# example: sh ./run_integration_test.sh <smoke|integration> <local|dev|uat|prod>
environment=$1
containerName="node-container"

docker stop node-container || true
docker rm node-container || true

# please see https://github.com/andrea-deri/prebuilt-img-yarn-base for yarn-testing-base image content
docker pull ${CONTAINER_REGISTRY}/yarn-testing-base:latest
docker run -dit --name ${containerName} ${CONTAINER_REGISTRY}/yarn-testing-base:latest

# run integration tests with yarn
docker cp -a ./src/. ${containerName}:/test
docker exec -i ${containerName} /bin/bash -c " \
cd ./test
export API_CONFIG_SUBSCRIPTION_KEY=${API_CONFIG_SUBSCRIPTION_KEY} \
export GPD_SUBSCRIPTION_KEY=${GPD_SUBSCRIPTION_KEY} \
export PAYMENTS_REST_SUBSCRIPTION_KEY=${PAYMENTS_REST_SUBSCRIPTION_KEY} \
export PAYMENTS_SOAP_SUBSCRIPTION_KEY=${PAYMENTS_SOAP_SUBSCRIPTION_KEY} \
export REPORTING_SUBSCRIPTION_KEY=${REPORTING_SUBSCRIPTION_KEY} \
export REPORTING_BATCH_CONNECTION_STRING=${REPORTING_BATCH_CONNECTION_STRING}
yarn test:${environment}"

# clean up container
docker stop ${containerName} && docker rm ${containerName}