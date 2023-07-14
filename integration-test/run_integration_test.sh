# example: sh ./run_integration_test.sh <local|dev|uat|prod> <sub-key>
set -e

export ENV=$1
export API_SUBSCRIPTION_KEY=$2

# run integration tests (application must be running)

cd ../integration-test/src || exit
yarn install
yarn test:$ENV
