#!/bin/bash
curl http://localhost:8080/v3/api-docs | python3 -m json.tool > ./openapi.json
