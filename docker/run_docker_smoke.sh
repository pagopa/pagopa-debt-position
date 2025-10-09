#!/bin/bash

ENV="local"

pip3 install yq

image="service-local:latest"
ENV="dev"
export image=${image}

FILE=.env
if test -f "$FILE"; then
    rm .env
fi

#!/bin/bash

config=$(yq -r '."microservice-chart".envConfig' ../helm/values-$ENV.yaml)
IFS=$'\n'
for line in $(echo "$config" | yq -r '. | to_entries[] | select(.key) | "\(.key)=\(.value)"'); do
    # Estrai la chiave e il valore dalla linea
    key=$(echo "$line" | cut -d'=' -f1)
    value=$(echo "$line" | cut -d'=' -f2-)

    # Se la chiave è SPRING_DATASOURCE_URL, assegna il valore specifico
    if [[ "$key" == "SPRING_DATASOURCE_URL" ]]; then
        value="jdbc:postgresql://pagopa-d-weu-gpd-pgflex.postgres.database.azure.com:6432/apd?sslmode=require&prepareThreshold=0&tcpKeepAlive=true"
    fi

    # Scrivi la chiave-valore nel file .env
    echo "$key=$value" >> .env
done


keyvault=$(yq  -r '."microservice-chart".keyvault.name' ../helm/values-$ENV.yaml)
secret=$(yq  -r '."microservice-chart".envSecret' ../helm/values-$ENV.yaml)
for line in $(echo "$secret" | yq -r '. | to_entries[] | select(.key) | "\(.key)=\(.value)"'); do
  IFS='=' read -r -a array <<< "$line"
  response=$(az keyvault secret show --vault-name $keyvault --name "${array[1]}")
  response=$(echo "$response" | tr -d '\n')
  value=$(echo "$response" | yq -r '.value')
  value=$(echo "$value" | sed 's/\$/\$\$/g')
  value=$(echo "$value" | tr -d '\n')
  echo "${array[0]}=$value" >> .env
done


stack_name=$(cd .. && basename "$PWD")
docker compose -f ./docker-compose-local.yml -p "${stack_name}" up -d --remove-orphans --force-recreate --build

# waiting the containers
printf 'Waiting for the service'
attempt_counter=0
max_attempts=50
until curl --head --silent --fail http://localhost:8080/actuator/health; do
  echo -n '.'
  sleep 5
  ((attempts++))
  if [ $attempts -gt 60 ]; then  # allunga a 5 minuti
    echo "Max attempts reached"
    docker compose ps
    docker logs --tail=200 gpd || true
    docker logs --tail=100 pgbouncer || true
    exit 1
  fi
done
echo 'Service Started'
