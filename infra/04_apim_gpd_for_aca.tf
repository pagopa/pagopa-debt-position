## API ##

resource "azurerm_api_management_api_version_set" "api_debt_positions_for_aca_api" {
  name                = format("%s-debt-positions-for-aca-service-api", var.env_short)
  api_management_name = local.apim.name
  resource_group_name = local.apim.rg
  display_name        = "GPD for ACA pagoPA - Debt Positions service API for ACA"
  versioning_scheme   = "Segment"
}

module "apim_api_debt_positions_for_aca_api_v1" {
  source = "git::https://github.com/pagopa/terraform-azurerm-v3.git//api_management_api?ref=v8.62.1"

  name                = format("%s-debt-positions-for-aca-service-api", local.product)
  api_management_name = local.apim.name
  resource_group_name = local.apim.rg
  product_ids         = [local.apim.aca_integration_product_id]

  subscription_required = true
  version_set_id        = azurerm_api_management_api_version_set.api_debt_positions_for_aca_api.id
  api_version           = "v1"

  description  = "API to support Debt Positions service for ACA"
  display_name = "GPD for ACA pagoPA - Debt Positions service API for ACA"
  path         = "aca/debt-positions-service"
  protocols    = ["https"]
  service_url  = local.gpd_core_service_url

  content_format = "openapi"
  // the content value is the GPD API v1
  content_value = templatefile("../openapi/openapi_aca_v1.json", {
    service = local.apim.aca_integration_product_id
  })

  xml_content = file("./api/aca_api/v1/_base_policy.xml")
}

#########################################
####  CREATE DEBT POSITION POLICIES  ####
#########################################

resource "terraform_data" "sha256_create_debt_position_v1_policy" {
  input = sha256(file("./api/create_base_policy.xml"))
}

resource "azurerm_api_management_api_operation_policy" "create_debt_position_v1_policy" {
  api_name            = format("%s-debt-positions-for-aca-service-api-v1", local.product)
  api_management_name = local.pagopa_apim_name
  resource_group_name = local.pagopa_apim_rg
  operation_id        = "createPosition"
  xml_content = templatefile("./api/create_base_policy.xml", {
    service_type_value = "ACA"
  })
}
