#!/bin/bash
set -euo pipefail

ENV="dev"

image="service-local:latest"

export image=${image}

FILE=.env
if test -f "$FILE"; then
  rm .env
fi

config=$(yq -r '."microservice-chart".envConfig' ../helm/values-$ENV.yaml)
IFS=$'\n'
for line in $(echo "$config" | yq -r '. | to_entries[] | select(.key) | "\(.key)=\(.value)"'); do
  # Estrai la chiave e il valore dalla linea
  key=$(echo "$line" | cut -d'=' -f1)
  value=$(echo "$line" | cut -d'=' -f2-)

    # Se la chiave è SPRING_DATASOURCE_URL, assegna il valore specifico
    if [[ "$key" == "SPRING_DATASOURCE_URL" ]]; then
        value="jdbc:postgresql://pagopa-d-weu-gpd-pgflex.postgres.database.azure.com:5432/apd?sslmode=require&prepareThreshold=0&tcpKeepAlive=true"
    fi

    # Scrivi la chiave-valore nel file .env
    echo "$key=$value" >> .env
done

keyvault=$(yq -r '."microservice-chart".keyvault.name' ../helm/values-$ENV.yaml)
secret=$(yq -r '."microservice-chart".envSecret' ../helm/values-$ENV.yaml)
for line in $(echo "$secret" | yq -r '. | to_entries[] | select(.key) | "\(.key)=\(.value)"'); do
  IFS='=' read -r -a array <<< "$line"
  response=$(az keyvault secret show --vault-name "$keyvault" --name "${array[1]}")
  response=$(echo "$response" | tr -d '\n')
  value=$(echo "$response" | yq -r '.value')
  value=$(echo "$value" | sed 's/\$/\$\$/g' | tr -d '\n')
  echo "${array[0]}=$value" >> .env
done

stack_name=$(cd .. && basename "$PWD")
docker compose -f ./docker-compose-local.yml -p "${stack_name}" up -d --remove-orphans --force-recreate --build


# waiting the containers
printf 'Waiting for the service'
attempts=0
max_attempts=60
while true; do
  rc=0
  err="$(curl -fsS -o /dev/null "http://localhost:8080/info" 2>&1)" || rc=$?
  if [ $rc -eq 0 ]; then
    echo 'Service Started'
    break
  fi

  if [ $rc -eq 56 ] || [[ "$err" == *"Recv failure"* ]]; then
    echo ' . waiting: app not ready yet (connection reset)'
  else
    echo " . waiting: $err"
  fi

  sleep 5
  attempts=$((attempts+1))
  if [ $attempts -ge $max_attempts ]; then
    echo " Max attempts reached"
    docker compose -f ./docker-compose-local.yml -p "${stack_name}" ps || true
    docker compose -f ./docker-compose-local.yml -p "${stack_name}" logs gpd || true
    docker compose -f ./docker-compose-local.yml -p "${stack_name}" logs pgbouncer || true
    # final check to show endpoint status
    curl -i "http://localhost:8080/info" || true
    exit 1
  fi
done
