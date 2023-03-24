# example: sh ./run_integration_test.sh <smoke|integration> <local|dev|uat|prod>

export APICONFIG_SUBSCRIPTION_KEY=$3
export GPD_SUBSCRIPTION_KEY=$4
export GPS_SUBSCRIPTION_KEY=$5
export DONATIONS_SUBSCRIPTION_KEY=$6
export IUVGENERATOR_SUBSCRIPTION_KEY=$7
export REST_PAYMENTS_SUBSCRIPTION_KEY=$8
export SOAP_PAYMENTS_SUBSCRIPTION_KEY=$9

TYPE=$1
ENV=$2

docker stop node-container && docker rm node-container
docker stop integration-node-container && docker rm integration-node-container

if [ "$TYPE" = "smoke" ]; then
  containerName="node-container"

  # create containers
  cd ../docker || exit
  sh ./run_docker.sh --test "$ENV"

  cd ../integration-test || exit

  if [ "$ENV" = "uat" ]; then
    sed -i 's/dev/uat/' src/config/.env.local
  elif [ "$ENV" = "prod" ]; then
    sed -i 's/dev.//' src/config/.env.local
  fi

else
  containerName="integration-node-container"
  test_type=:$ENV

  docker pull ${containerRegistry}/yarn-testing-base:latest
  docker run -dit --name ${containerName} ${containerRegistry}/yarn-testing-base:latest
fi

# run integration tests with yarn
docker cp -a ./src/. ${containerName}:/test
docker exec -i ${containerName} /bin/bash -c " \
cd ./test
export APICONFIG_SUBSCRIPTION_KEY=$3 \
export GPD_SUBSCRIPTION_KEY=$4 \
export GPS_SUBSCRIPTION_KEY=$5 \
export DONATIONS_SUBSCRIPTION_KEY=$6 \
export IUVGENERATOR_SUBSCRIPTION_KEY=$7 \
export REST_PAYMENTS_SUBSCRIPTION_KEY=$8 \
export SOAP_PAYMENTS_SUBSCRIPTION_KEY=$9 && \
yarn test${test_type}"

# clean up container
docker stop ${containerName} && docker rm ${containerName}