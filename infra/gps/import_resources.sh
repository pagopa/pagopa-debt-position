#!/bin/bash
# Generated with `generate_imports.py`

# resource.azurerm_api_management_api_version_set.api_gpd_api
echo 'Importing azurerm_api_management_api_version_set.api_gpd_api'
./terraform.sh import weu-dev 'azurerm_api_management_api_version_set.api_gpd_api' '/subscriptions/bbe47ad4-08b3-4925-94c5-1278e5819b86/resourceGroups/pagopa-d-api-rg/providers/Microsoft.ApiManagement/service/pagopa-d-apim/apiVersionSets/d-api-gpd-api'


# module.apim_api_gpd_api
echo 'Importing module.apim_api_gpd_api.azurerm_api_management_api.this'
./terraform.sh import weu-dev 'module.apim_api_gpd_api.azurerm_api_management_api.this' '/subscriptions/bbe47ad4-08b3-4925-94c5-1278e5819b86/resourceGroups/pagopa-d-api-rg/providers/Microsoft.ApiManagement/service/pagopa-d-apim/apis/d-api-gpd-api-v1;rev=1'


# module.apim_api_gpd_api
echo 'Importing module.apim_api_gpd_api.azurerm_api_management_api_policy.this[0]'
./terraform.sh import weu-dev 'module.apim_api_gpd_api.azurerm_api_management_api_policy.this[0]' '/subscriptions/bbe47ad4-08b3-4925-94c5-1278e5819b86/resourceGroups/pagopa-d-api-rg/providers/Microsoft.ApiManagement/service/pagopa-d-apim/apis/d-api-gpd-api-v1'


# module.apim_api_gpd_api
echo 'Importing module.apim_api_gpd_api.azurerm_api_management_product_api.this["product-gpd"]'
./terraform.sh import weu-dev 'module.apim_api_gpd_api.azurerm_api_management_product_api.this["product-gpd"]' '/subscriptions/bbe47ad4-08b3-4925-94c5-1278e5819b86/resourceGroups/pagopa-d-api-rg/providers/Microsoft.ApiManagement/service/pagopa-d-apim/products/product-gpd/apis/d-api-gpd-api-v1'


# module.apim_api_gpd_api_v2
echo 'Importing module.apim_api_gpd_api_v2.azurerm_api_management_api.this'
./terraform.sh import weu-dev 'module.apim_api_gpd_api_v2.azurerm_api_management_api.this' '/subscriptions/bbe47ad4-08b3-4925-94c5-1278e5819b86/resourceGroups/pagopa-d-api-rg/providers/Microsoft.ApiManagement/service/pagopa-d-apim/apis/d-api-gpd-api-v2;rev=1'


# module.apim_api_gpd_api_v2
echo 'Importing module.apim_api_gpd_api_v2.azurerm_api_management_api_policy.this[0]'
./terraform.sh import weu-dev 'module.apim_api_gpd_api_v2.azurerm_api_management_api_policy.this[0]' '/subscriptions/bbe47ad4-08b3-4925-94c5-1278e5819b86/resourceGroups/pagopa-d-api-rg/providers/Microsoft.ApiManagement/service/pagopa-d-apim/apis/d-api-gpd-api-v2'


# module.apim_api_gpd_api_v2
echo 'Importing module.apim_api_gpd_api_v2.azurerm_api_management_product_api.this["product-gpd"]'
./terraform.sh import weu-dev 'module.apim_api_gpd_api_v2.azurerm_api_management_product_api.this["product-gpd"]' '/subscriptions/bbe47ad4-08b3-4925-94c5-1278e5819b86/resourceGroups/pagopa-d-api-rg/providers/Microsoft.ApiManagement/service/pagopa-d-apim/products/product-gpd/apis/d-api-gpd-api-v2'


# resource.azurerm_api_management_api_version_set.api_debt_positions_api
echo 'Importing azurerm_api_management_api_version_set.api_debt_positions_api'
./terraform.sh import weu-dev 'azurerm_api_management_api_version_set.api_debt_positions_api' '/subscriptions/bbe47ad4-08b3-4925-94c5-1278e5819b86/resourceGroups/pagopa-d-api-rg/providers/Microsoft.ApiManagement/service/pagopa-d-apim/apiVersionSets/d-debt-positions-service-api'


# module.apim_api_debt_positions_api_v1
echo 'Importing module.apim_api_debt_positions_api_v1.azurerm_api_management_api.this'
./terraform.sh import weu-dev 'module.apim_api_debt_positions_api_v1.azurerm_api_management_api.this' '/subscriptions/bbe47ad4-08b3-4925-94c5-1278e5819b86/resourceGroups/pagopa-d-api-rg/providers/Microsoft.ApiManagement/service/pagopa-d-apim/apis/pagopa-d-debt-positions-service-api-v1;rev=1'


# module.apim_api_debt_positions_api_v1
echo 'Importing module.apim_api_debt_positions_api_v1.azurerm_api_management_api_policy.this[0]'
./terraform.sh import weu-dev 'module.apim_api_debt_positions_api_v1.azurerm_api_management_api_policy.this[0]' '/subscriptions/bbe47ad4-08b3-4925-94c5-1278e5819b86/resourceGroups/pagopa-d-api-rg/providers/Microsoft.ApiManagement/service/pagopa-d-apim/apis/pagopa-d-debt-positions-service-api-v1'


# module.apim_api_debt_positions_api_v1
echo 'Importing module.apim_api_debt_positions_api_v1.azurerm_api_management_product_api.this["aca-integration"]'
./terraform.sh import weu-dev 'module.apim_api_debt_positions_api_v1.azurerm_api_management_product_api.this["aca-integration"]' '/subscriptions/bbe47ad4-08b3-4925-94c5-1278e5819b86/resourceGroups/pagopa-d-api-rg/providers/Microsoft.ApiManagement/service/pagopa-d-apim/products/aca-integration/apis/pagopa-d-debt-positions-service-api-v1'


# module.apim_api_debt_positions_api_v1
echo 'Importing module.apim_api_debt_positions_api_v1.azurerm_api_management_product_api.this["debt-positions"]'
./terraform.sh import weu-dev 'module.apim_api_debt_positions_api_v1.azurerm_api_management_product_api.this["debt-positions"]' '/subscriptions/bbe47ad4-08b3-4925-94c5-1278e5819b86/resourceGroups/pagopa-d-api-rg/providers/Microsoft.ApiManagement/service/pagopa-d-apim/products/debt-positions/apis/pagopa-d-debt-positions-service-api-v1'


# module.apim_api_debt_positions_api_v1
echo 'Importing module.apim_api_debt_positions_api_v1.azurerm_api_management_product_api.this["debt-positions-integration"]'
./terraform.sh import weu-dev 'module.apim_api_debt_positions_api_v1.azurerm_api_management_product_api.this["debt-positions-integration"]' '/subscriptions/bbe47ad4-08b3-4925-94c5-1278e5819b86/resourceGroups/pagopa-d-api-rg/providers/Microsoft.ApiManagement/service/pagopa-d-apim/products/debt-positions-integration/apis/pagopa-d-debt-positions-service-api-v1'


# module.apim_api_debt_positions_api_v2[0]
echo 'Importing module.apim_api_debt_positions_api_v2[0].azurerm_api_management_api.this'
./terraform.sh import weu-dev 'module.apim_api_debt_positions_api_v2[0].azurerm_api_management_api.this' '/subscriptions/bbe47ad4-08b3-4925-94c5-1278e5819b86/resourceGroups/pagopa-d-api-rg/providers/Microsoft.ApiManagement/service/pagopa-d-apim/apis/pagopa-d-debt-positions-service-api-v2;rev=1'


# module.apim_api_debt_positions_api_v2[0]
echo 'Importing module.apim_api_debt_positions_api_v2[0].azurerm_api_management_api_policy.this[0]'
./terraform.sh import weu-dev 'module.apim_api_debt_positions_api_v2[0].azurerm_api_management_api_policy.this[0]' '/subscriptions/bbe47ad4-08b3-4925-94c5-1278e5819b86/resourceGroups/pagopa-d-api-rg/providers/Microsoft.ApiManagement/service/pagopa-d-apim/apis/pagopa-d-debt-positions-service-api-v2'


# module.apim_api_debt_positions_api_v2[0]
echo 'Importing module.apim_api_debt_positions_api_v2[0].azurerm_api_management_product_api.this["debt-positions"]'
./terraform.sh import weu-dev 'module.apim_api_debt_positions_api_v2[0].azurerm_api_management_product_api.this["debt-positions"]' '/subscriptions/bbe47ad4-08b3-4925-94c5-1278e5819b86/resourceGroups/pagopa-d-api-rg/providers/Microsoft.ApiManagement/service/pagopa-d-apim/products/debt-positions/apis/pagopa-d-debt-positions-service-api-v2'


# module.apim_api_debt_positions_api_v2[0]
echo 'Importing module.apim_api_debt_positions_api_v2[0].azurerm_api_management_product_api.this["debt-positions-integration"]'
./terraform.sh import weu-dev 'module.apim_api_debt_positions_api_v2[0].azurerm_api_management_product_api.this["debt-positions-integration"]' '/subscriptions/bbe47ad4-08b3-4925-94c5-1278e5819b86/resourceGroups/pagopa-d-api-rg/providers/Microsoft.ApiManagement/service/pagopa-d-apim/products/debt-positions-integration/apis/pagopa-d-debt-positions-service-api-v2'


# module.apim_api_debt_positions_api_v3
echo 'Importing module.apim_api_debt_positions_api_v3.azurerm_api_management_api.this'
./terraform.sh import weu-dev 'module.apim_api_debt_positions_api_v3.azurerm_api_management_api.this' '/subscriptions/bbe47ad4-08b3-4925-94c5-1278e5819b86/resourceGroups/pagopa-d-api-rg/providers/Microsoft.ApiManagement/service/pagopa-d-apim/apis/pagopa-d-debt-positions-service-api-v3;rev=1'


# module.apim_api_debt_positions_api_v3
echo 'Importing module.apim_api_debt_positions_api_v3.azurerm_api_management_api_policy.this[0]'
./terraform.sh import weu-dev 'module.apim_api_debt_positions_api_v3.azurerm_api_management_api_policy.this[0]' '/subscriptions/bbe47ad4-08b3-4925-94c5-1278e5819b86/resourceGroups/pagopa-d-api-rg/providers/Microsoft.ApiManagement/service/pagopa-d-apim/apis/pagopa-d-debt-positions-service-api-v3'


# module.apim_api_debt_positions_api_v3
echo 'Importing module.apim_api_debt_positions_api_v3.azurerm_api_management_product_api.this["debt-positions"]'
./terraform.sh import weu-dev 'module.apim_api_debt_positions_api_v3.azurerm_api_management_product_api.this["debt-positions"]' '/subscriptions/bbe47ad4-08b3-4925-94c5-1278e5819b86/resourceGroups/pagopa-d-api-rg/providers/Microsoft.ApiManagement/service/pagopa-d-apim/products/debt-positions/apis/pagopa-d-debt-positions-service-api-v3'


# module.apim_api_debt_positions_api_v3
echo 'Importing module.apim_api_debt_positions_api_v3.azurerm_api_management_product_api.this["debt-positions-integration"]'
./terraform.sh import weu-dev 'module.apim_api_debt_positions_api_v3.azurerm_api_management_product_api.this["debt-positions-integration"]' '/subscriptions/bbe47ad4-08b3-4925-94c5-1278e5819b86/resourceGroups/pagopa-d-api-rg/providers/Microsoft.ApiManagement/service/pagopa-d-apim/products/debt-positions-integration/apis/pagopa-d-debt-positions-service-api-v3'


# resource.terraform_data.sha256_create_debt_position_v1_policy
echo 'Importing terraform_data.sha256_create_debt_position_v1_policy'
./terraform.sh import weu-dev 'terraform_data.sha256_create_debt_position_v1_policy' '486f028e-f9e6-fc91-00b1-0e921f75c57e'


# resource.azurerm_api_management_api_operation_policy.create_debt_position_v1_policy
echo 'Importing azurerm_api_management_api_operation_policy.create_debt_position_v1_policy'
./terraform.sh import weu-dev 'azurerm_api_management_api_operation_policy.create_debt_position_v1_policy' '/subscriptions/bbe47ad4-08b3-4925-94c5-1278e5819b86/resourceGroups/pagopa-d-api-rg/providers/Microsoft.ApiManagement/service/pagopa-d-apim/apis/pagopa-d-debt-positions-service-api-v1/operations/createPosition'


# resource.terraform_data.sha256_create_debt_position_v2_policy[0]
echo 'Importing terraform_data.sha256_create_debt_position_v2_policy[0]'
./terraform.sh import weu-dev 'terraform_data.sha256_create_debt_position_v2_policy[0]' 'b24352ca-35b1-cc7f-9a34-e9374b24ee22'


# resource.azurerm_api_management_api_operation_policy.create_debt_position_v2_policy[0]
echo 'Importing azurerm_api_management_api_operation_policy.create_debt_position_v2_policy[0]'
./terraform.sh import weu-dev 'azurerm_api_management_api_operation_policy.create_debt_position_v2_policy[0]' '/subscriptions/bbe47ad4-08b3-4925-94c5-1278e5819b86/resourceGroups/pagopa-d-api-rg/providers/Microsoft.ApiManagement/service/pagopa-d-apim/apis/pagopa-d-debt-positions-service-api-v2/operations/createMultiplePositions'


# resource.azurerm_api_management_api_version_set.api_pn_integration_api
echo 'Importing azurerm_api_management_api_version_set.api_pn_integration_api'
./terraform.sh import weu-dev 'azurerm_api_management_api_version_set.api_pn_integration_api' '/subscriptions/bbe47ad4-08b3-4925-94c5-1278e5819b86/resourceGroups/pagopa-d-api-rg/providers/Microsoft.ApiManagement/service/pagopa-d-apim/apiVersionSets/d-pn-integration-rest-api'


# module.apim_api_pn_integration_gpd_api_v1
echo 'Importing module.apim_api_pn_integration_gpd_api_v1.azurerm_api_management_api.this'
./terraform.sh import weu-dev 'module.apim_api_pn_integration_gpd_api_v1.azurerm_api_management_api.this' '/subscriptions/bbe47ad4-08b3-4925-94c5-1278e5819b86/resourceGroups/pagopa-d-api-rg/providers/Microsoft.ApiManagement/service/pagopa-d-apim/apis/d-pn-integration-gpd-api-aks-v1;rev=1'


# module.apim_api_pn_integration_gpd_api_v1
echo 'Importing module.apim_api_pn_integration_gpd_api_v1.azurerm_api_management_api_policy.this[0]'
./terraform.sh import weu-dev 'module.apim_api_pn_integration_gpd_api_v1.azurerm_api_management_api_policy.this[0]' '/subscriptions/bbe47ad4-08b3-4925-94c5-1278e5819b86/resourceGroups/pagopa-d-api-rg/providers/Microsoft.ApiManagement/service/pagopa-d-apim/apis/d-pn-integration-gpd-api-aks-v1'


# module.apim_api_pn_integration_gpd_api_v1
echo 'Importing module.apim_api_pn_integration_gpd_api_v1.azurerm_api_management_product_api.this["pn-integration"]'
./terraform.sh import weu-dev 'module.apim_api_pn_integration_gpd_api_v1.azurerm_api_management_product_api.this["pn-integration"]' '/subscriptions/bbe47ad4-08b3-4925-94c5-1278e5819b86/resourceGroups/pagopa-d-api-rg/providers/Microsoft.ApiManagement/service/pagopa-d-apim/products/pn-integration/apis/d-pn-integration-gpd-api-aks-v1'


echo 'Import executed succesfully on weu-dev environment! ⚡'
