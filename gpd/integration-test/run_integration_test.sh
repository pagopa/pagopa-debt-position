# example: sh ./run_integration_test.sh <local|dev|uat|prod> <sub-key>
set -e

# create containers
cd ../docker || exit
sh ./run_docker.sh "$1"

# run integration tests
export subkey=$2
cd ../integration-test/src || exit
yarn install
yarn test
