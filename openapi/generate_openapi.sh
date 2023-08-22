#!/bin/bash

if [[ "$(pwd)" =~ .*"openapi".* ]]; then
    cd ..
fi

mvn test -Dtest=OpenApiGenerationTest