####################
# GPD INTERNAL USE #
####################

## API ##

resource "azurerm_api_management_api_version_set" "api_gpd_api" {
  name                = format("%s-api-gpd-api", var.env_short)
  api_management_name = local.apim.name
  resource_group_name = local.apim.rg
  display_name        = "Gestione Posizione Debitorie"
  versioning_scheme   = "Segment"
}

# Internal v1
module "apim_api_gpd_api" {
  source = "git::https://github.com/pagopa/terraform-azurerm-v3.git//api_management_api?ref=v8.62.1"

  name                = format("%s-api-gpd-api", var.env_short)
  api_management_name = local.apim.name
  resource_group_name = local.apim.rg
  product_ids = [local.apim.internal_gpd_product_id] // TODO verify
  # TODO chiedere a Pasquale
  # concat([local.apim.gpd_product_product_id],
  # var.env_short == "-" ? [] : [module.apim_gpd_payments_pull_product_and_debt_positions_product_test[0].product_id]) # ppull-prod-test

  subscription_required = false
  api_version           = "v1"
  version_set_id        = azurerm_api_management_api_version_set.api_gpd_api.id
  service_url           = local.gpd_core_service_url

  description  = "Api Gestione Posizione Debitorie"
  display_name = "GPD pagoPA"
  path         = "gpd/api"
  protocols    = ["https"]

  content_format = "openapi"
  content_value = templatefile("../../openapi/openapi_internal_v1.json", {
    service = local.apim.internal_gpd_product_id
  })

  xml_content = file("./api/internal_api/v1/_base_policy.xml")
}

# Internal v2
module "apim_api_gpd_api_v2" {
  source = "git::https://github.com/pagopa/terraform-azurerm-v3.git//api_management_api?ref=v8.62.1"

  name                = "${var.env_short}-api-gpd-api"
  api_management_name = local.apim.name
  resource_group_name = local.apim.rg
  product_ids = [local.apim.internal_gpd_product_id] // TODO verify
  # concat([local.apim.gpd_product_product_id],
  # var.env_short == "-" ? [] : [module.apim_gpd_payments_pull_product_and_debt_positions_product_test[0].product_id]) # ppull-prod-test

  subscription_required = false
  api_version           = "v2"
  version_set_id        = azurerm_api_management_api_version_set.api_gpd_api.id
  service_url           = local.gpd_core_service_url

  description  = "Api Gestione Posizione Debitorie"
  display_name = "GPD pagoPA"
  path         = "gpd/api"
  protocols    = ["https"]

  content_format = "openapi"
  content_value = templatefile("../../openapi/openapi_internal_v2.json", {
    service = local.apim.internal_gpd_product_id
  })

  xml_content = file("./api/internal_api/v2/_base_policy.xml")
  # ⚠️ The API base policy contains the logic for bulk endpoint.
  # ⚠️ It would have been better to make the policy on the specific operation.
}

####################
# GPD EXTERNAL USE #
####################

## API ##

resource "azurerm_api_management_api_version_set" "api_debt_positions_api" {
  name                = format("%s-debt-positions-service-api", var.env_short)
  api_management_name = local.apim.name
  resource_group_name = local.apim.rg
  display_name        = "GPD pagoPA - Debt Positions service API for organizations"
  versioning_scheme   = "Segment"
}

# External v1
module "apim_api_debt_positions_api_v1" {
  source = "git::https://github.com/pagopa/terraform-azurerm-v3.git//api_management_api?ref=v8.62.1"

  name                = format("%s-debt-positions-service-api", local.product)
  api_management_name = local.apim.name
  resource_group_name = local.apim.rg
  product_ids         = [local.apim.external_gpd_product_id, local.apim.aca_integration_product_id, local.apim.gpd_integration_product_id]
  # ⚠️ local.apim.external_gpd_product_id should be deprecated!!!

  subscription_required = true
  version_set_id        = azurerm_api_management_api_version_set.api_debt_positions_api.id
  api_version           = "v1"

  description  = local.external.description
  display_name = local.external.display_name
  path         = local.external.path
  protocols    = ["https"]
  service_url  = local.gpd_core_service_url

  content_format = "openapi"
  content_value = templatefile("../../openapi/openapi_external_v1.json", {
    service = local.apim.external_gpd_product_id
  })

  xml_content = file("./api/external_api/v1/_base_policy.xml")
}

# External v2
module "apim_api_debt_positions_api_v2" {
  count  = var.env_short != "p" ? 1 : 0 # disabled v2 external bulk prod
  source = "git::https://github.com/pagopa/terraform-azurerm-v3.git//api_management_api?ref=v8.62.1"

  name                  = format("%s-debt-positions-service-api", local.product)
  api_management_name = local.apim.name
  resource_group_name = local.apim.rg
  product_ids           = [local.apim.external_gpd_product_id, local.apim.gpd_integration_product_id]
  subscription_required = true
  version_set_id        = azurerm_api_management_api_version_set.api_debt_positions_api.id
  api_version           = "v2"

  description  = local.external.description
  display_name = local.external.display_name
  path         = local.external.path
  protocols    = ["https"]
  service_url  = local.gpd_core_service_url

  content_format = "openapi"
  content_value = templatefile("../../openapi/openapi_external_v2.json", {
    service = local.apim.external_gpd_product_id
  })
  // warning: ad-hoc base policy because there is a rewrite URI
  xml_content = file("./api/external_api/v2/_base_policy.xml")
}

# External v3
module "apim_api_debt_positions_api_v3" {
  source = "git::https://github.com/pagopa/terraform-azurerm-v3.git//api_management_api?ref=v8.62.1"

  name                = format("%s-debt-positions-service-api", local.product)
  api_management_name = local.apim.name
  resource_group_name = local.apim.rg
  product_ids         = [local.apim.external_gpd_product_id, local.apim.gpd_integration_product_id]

  subscription_required = true
  version_set_id        = azurerm_api_management_api_version_set.api_debt_positions_api.id
  api_version           = "v3"

  description  = local.external.description
  display_name = local.external.display_name
  path         = local.external.path
  protocols    = ["https"]
  service_url  = "${local.gpd_core_service_url}/v3"

  content_format = "openapi"
  content_value = templatefile("../../openapi/openapi_external_v3.json", {
    service = local.apim.external_gpd_product_id
  })

  xml_content = file("./api/external_api/v3/_base_policy.xml") # TODO rivedere, c'è logica di policy
}

#########################################
## GPD CREATE DEBT POSITION POLICIES ####
#########################################

# v1
resource "terraform_data" "sha256_create_debt_position_v1_policy" {
  input = sha256(file("./api/create_base_policy.xml"))
}

resource "azurerm_api_management_api_operation_policy" "create_debt_position_v1_policy" {
  api_name            = format("%s-debt-positions-service-api-v1", local.product)
  api_management_name = local.apim.name
  resource_group_name = local.apim.rg
  operation_id        = "createPosition"
  xml_content = templatefile("./api/create_base_policy.xml", {
    service_type_value = "GPD"
  })
}

# v2
resource "terraform_data" "sha256_create_debt_position_v2_policy" {
  count = var.env_short != "p" ? 1 : 0 # disabled v2 external bulk prod
  input = sha256(file("./api/create_base_policy.xml"))
}

resource "azurerm_api_management_api_operation_policy" "create_debt_position_v2_policy" {
  count = var.env_short != "p" ? 1 : 0 # disabled v2 external bulk prod

  api_name            = format("%s-debt-positions-service-api-v2", local.product)
  api_management_name = local.apim.name
  resource_group_name = local.apim.rg
  operation_id        = "createMultiplePositions"
  xml_content = templatefile("./api/create_base_policy.xml", {
    service_type_value = "GPD"
  })
}