#!/bin/bash
# Generated with `generate_imports.py`

# resource.azurerm_api_management_api_version_set.api_debt_positions_for_aca_api
echo 'Importing azurerm_api_management_api_version_set.api_debt_positions_for_aca_api'
./terraform.sh import weu-dev 'azurerm_api_management_api_version_set.api_debt_positions_for_aca_api' '/subscriptions/bbe47ad4-08b3-4925-94c5-1278e5819b86/resourceGroups/pagopa-d-api-rg/providers/Microsoft.ApiManagement/service/pagopa-d-apim/apiVersionSets/d-debt-positions-for-aca-service-api'


# module.apim_api_debt_positions_for_aca_api_v1
echo 'Importing module.apim_api_debt_positions_for_aca_api_v1.azurerm_api_management_api.this'
./terraform.sh import weu-dev 'module.apim_api_debt_positions_for_aca_api_v1.azurerm_api_management_api.this' '/subscriptions/bbe47ad4-08b3-4925-94c5-1278e5819b86/resourceGroups/pagopa-d-api-rg/providers/Microsoft.ApiManagement/service/pagopa-d-apim/apis/pagopa-d-debt-positions-for-aca-service-api-v1;rev=1'


# module.apim_api_debt_positions_for_aca_api_v1
echo 'Importing module.apim_api_debt_positions_for_aca_api_v1.azurerm_api_management_api_policy.this[0]'
./terraform.sh import weu-dev 'module.apim_api_debt_positions_for_aca_api_v1.azurerm_api_management_api_policy.this[0]' '/subscriptions/bbe47ad4-08b3-4925-94c5-1278e5819b86/resourceGroups/pagopa-d-api-rg/providers/Microsoft.ApiManagement/service/pagopa-d-apim/apis/pagopa-d-debt-positions-for-aca-service-api-v1'


# module.apim_api_debt_positions_for_aca_api_v1
echo 'Importing module.apim_api_debt_positions_for_aca_api_v1.azurerm_api_management_product_api.this["aca"]'
./terraform.sh import weu-dev 'module.apim_api_debt_positions_for_aca_api_v1.azurerm_api_management_product_api.this["aca"]' '/subscriptions/bbe47ad4-08b3-4925-94c5-1278e5819b86/resourceGroups/pagopa-d-api-rg/providers/Microsoft.ApiManagement/service/pagopa-d-apim/products/aca/apis/pagopa-d-debt-positions-for-aca-service-api-v1'


# resource.terraform_data.sha256_create_debt_position_v1_policy
echo 'Importing terraform_data.sha256_create_debt_position_v1_policy'
./terraform.sh import weu-dev 'terraform_data.sha256_create_debt_position_v1_policy' '927f5256-34f7-4936-7ae4-ca5501032013'


# resource.azurerm_api_management_api_operation_policy.create_debt_position_v1_policy
echo 'Importing azurerm_api_management_api_operation_policy.create_debt_position_v1_policy'
./terraform.sh import weu-dev 'azurerm_api_management_api_operation_policy.create_debt_position_v1_policy' '/subscriptions/bbe47ad4-08b3-4925-94c5-1278e5819b86/resourceGroups/pagopa-d-api-rg/providers/Microsoft.ApiManagement/service/pagopa-d-apim/apis/pagopa-d-debt-positions-for-aca-service-api-v1/operations/createPosition'


echo 'Import executed succesfully on weu-dev environment! ⚡'
