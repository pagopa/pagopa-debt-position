#!/bin/bash

if [[ "$(pwd)" =~ .*"openapi".* ]]; then
    cd ..
fi

if ! $(curl --output /dev/null --silent --head --fail http://localhost:8080/info); then
  mvn spring-boot:run -Dmaven.test.skip=true -Dspring-boot.run.profiles=h2 &
fi

# waiting the service
printf 'Waiting for the service'
attempt_counter=0
max_attempts=50
until $(curl --output /dev/null --silent --head --fail http://localhost:8080/info); do
    if [ ${attempt_counter} -eq ${max_attempts} ];then
      echo "Max attempts reached"
      exit 1
    fi

    printf '.'
    attempt_counter=$((attempt_counter+1))
    sleep 5
done
echo 'Service Started'

curl http://localhost:8080/v3/api-docs > ./openapi/openapi_internal.json
curl http://localhost:8080/v3/api-docs/external > ./openapi/openapi_external.json