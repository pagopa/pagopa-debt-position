#!/bin/bash

STAGE=$1
EXEC=$2
case $STAGE in

  d) # develop
    HOSTPORT="http://localhost:8085"
    BASEPATH="/"
    ;;

  u) # uat
    HOSTPORT="http://localhost:8085"
    BASEPATH="/"
    ;;

  p) # production
    HOSTPORT="http://localhost:8085"
    BASEPATH="/"
    ;;

  *) # local
    HOSTPORT="http://localhost:8085"
    BASEPATH="/"
    ;;
esac

if [ -z "$EXEC" ]
then
  echo "Warning: No test run : select STAGE (d=DEV,u=UAT,p=PROD,l=local) and ENV (int=integration,load=k6) see example below !"
  echo "$0 l int"
else
  if [[ "$EXEC" =~ ^(int|load)$ ]]; then
      if [ "$EXEC" = "load" ]
      then
          postman-to-k6 "api-test/Debt Position.postman_collection.json" --environment "api-test/Debt Position - Local.postman_environment.json" -o ./k6-script.js
          k6 run --http-debug="full" --out csv=test.csv --vus 1 --duration 1s ./k6-script.js
      else
          newman run "api-test/Debt Position.postman_collection.json" --environment="api-test/Debt Position - Local.postman_environment.json" --reporters cli,junit --reporter-junit-export Results/DebtPosition-TEST.xml
      fi
  else
      echo "Warning: No test run : select STAGE (d=DEV,u=UAT,p=PROD,l=local) and ENV (int=integration,load=k6) see example below !"
      echo "$0 l int"
      exit 0
  fi
fi


