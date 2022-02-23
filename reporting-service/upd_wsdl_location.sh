#!/bin/bash

if [ -z ${NODO_HOST} ]; then NODO_HOST="http://localhost:8086/nodo-per-pa/v1"; fi

sed "s|NODO_HOST|${NODO_HOST}|g" src/main/resources/wsdl/NodoPerPa.wsdl.TEMPLATE > src/main/resources/wsdl/NodoPerPa.wsdl