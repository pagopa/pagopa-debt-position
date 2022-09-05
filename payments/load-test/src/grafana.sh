git clone https://github.com/grafana/k6 && cd k6
git submodule update --init
docker-compose up
cd ..
k6 run --env VARS=dev.environment.json --env TEST_TYPE=./test-types/load.json -o influxdb=http://localhost:8086/k6 ./payments_demand_notice.js
