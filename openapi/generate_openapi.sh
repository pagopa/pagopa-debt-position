#!/bin/bash

if [[ "$(pwd)" =~ .*"openapi".* ]]; then
    cd ..
fi

mvn test -Dtest=OpenApiGenerationTest

# internal openapi
# v2: changing single to massive create operation
jq 'del( .paths["/organizations/{organizationfiscalcode}/debtpositions"] | .post ) | .paths["/organizations/{organizationfiscalcode}/debtpositions"].post = .paths["/organizations/{organizationfiscalcode}/debtpositions/bulk"].post | del( .paths["/organizations/{organizationfiscalcode}/debtpositions/bulk"] )' ./openapi/openapi_internal.json > ./openapi/openapi_internal_massive.json

# v1
jq 'del( .paths["/organizations/{organizationfiscalcode}/debtpositions"] | .put, .delete ) | .paths["/organizations/{organizationfiscalcode}/debtpositions"].post = .paths["/organizations/{organizationfiscalcode}/debtpositions/bulk"].post | del( .paths["/organizations/{organizationfiscalcode}/debtpositions/bulk"] )' ./openapi/openapi_internal.json > ./openapi/openapi_internal.json.temp && mv ./openapi/openapi_internal.json.temp ./openapi/openapi_internal.json


# external openapi
# v1: deleting bulk update and delete from external json
jq 'del(.paths["/organizations/{organizationfiscalcode}/debtpositions"] | .put, .delete)' ./openapi/openapi_external.json > ./openapi/openapi_external.json.temp && mv ./openapi/openapi_external.json.temp ./openapi/openapi_external.json

# v2: removing single create and get operations from external massive json
jq 'del( .paths["/organizations/{organizationfiscalcode}/debtpositions"] | .post, .get ) | .paths["/organizations/{organizationfiscalcode}/debtpositions"].post = .paths["/organizations/{organizationfiscalcode}/debtpositions/bulk"].post | del( .paths["/organizations/{organizationfiscalcode}/debtpositions/bulk"])' ./openapi/openapi_external_massive.json > ./openapi/openapi_external_massive.json.temp && mv ./openapi/openapi_external_massive.json.temp ./openapi/openapi_external_massive.json

