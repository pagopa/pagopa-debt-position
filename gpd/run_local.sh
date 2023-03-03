#!/bin/bash

# kill a previous instance if exists
lsof -i :8085 | awk '{print $2}' | tail -n +2 | xargs kill -9

# set local env
export $(grep -v '^#' .env.local | xargs)

# build & run db
docker-compose -f ../docker-compose.yml up --build -d postgres

# build
# mvn clean package -DskipTests

# run
mvn spring-boot:run -Dspring-boot.run.arguments=--server.port=8085 -Dspring-boot.run.profiles=local &

# check if GPD is UP and open api spec
URL_CHECK="http://localhost:8085/v3/api-docs"
CMD_CHECK=`curl -o /dev/null -s -w "%{http_code}\n" $URL_CHECK`
while [ "$CMD_CHECK" != "200" ]
do
    sleep 3
    echo -n "."
    CMD_CHECK=`curl -o /dev/null -s -w "%{http_code}\n" $URL_CHECK`
done

# open -na "Google Chrome" --args -incognito http://localhost:8085/swagger-ui/index.html
# open http://localhost:8085/swagger-ui/index.html