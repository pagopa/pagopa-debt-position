#!/bin/bash

if [[ "$(pwd)" =~ .*"openapi".* ]]; then
    cd ..
fi

mvn test -Dtest=OpenApiGenerationTest

# deleting bulk update and delete from external json
jq 'del(.paths["/organizations/{organizationfiscalcode}/debtpositions"] | .put, .delete)' ./openapi/openapi_external.json > ./openapi/openapi_external.json.temp && mv ./openapi/openapi_external.json.temp ./openapi/openapi_external.json

# removing single create and get operations from external massive json
jq 'del( .paths["/organizations/{organizationfiscalcode}/debtpositions"] | .post, .get ) | .paths["/organizations/{organizationfiscalcode}/debtpositions"].post = .paths["/organizations/{organizationfiscalcode}/debtpositions/bulk"].post | del( .paths["/organizations/{organizationfiscalcode}/debtpositions/bulk"])' ./openapi/openapi_external_massive.json > ./openapi/openapi_external_massive.json.temp && mv ./openapi/openapi_external_massive.json.temp ./openapi/openapi_external_massive.json

