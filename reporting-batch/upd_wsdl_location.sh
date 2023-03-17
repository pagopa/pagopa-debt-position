#!/bin/bash

NODO_HOST=$1
echo "Passed NODO_HOST parameter value: $NODO_HOST"

if [ -z "$NODO_HOST" ]; then
  echo "No NODO_HOST variable passed. Defining a default value..."
  NODO_HOST="http://localhost:8086/nodo-per-pa/v1";
fi

echo "Defined NODO_HOST as location for SOAP endpoints: $NODO_HOST"

sed "s|NODO_HOST|$NODO_HOST|g" src/main/resources/wsdl/NodoPerPa.wsdl.TEMPLATE > src/main/resources/wsdl/NodoPerPa.wsdl