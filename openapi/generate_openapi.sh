#!/bin/bash
curl http://localhost:8080/v3/api-docs/internal > ./openapi_internal.json
curl http://localhost:8080/v3/api-docs/external > ./openapi_external.json