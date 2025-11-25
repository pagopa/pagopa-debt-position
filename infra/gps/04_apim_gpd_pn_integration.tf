####################
## Local variables #
####################

locals {
  apim_pn_integration_rest_api = {
    display_name          = 
    description           = "GPD REST API for PN for retrieve payment options and update payment option fee"
    published             = false
    subscription_required = true
    approval_required     = true
    subscriptions_limit   = 1000
    service_url           = local.gpd_core_service_url
    gpd_service = {
      display_name = "GPD PN Integration"
      description  = "GPD API per Piattaforma Notifiche"
      path         = "pn-integration-gpd/api"
    }
  }
}

##############
## REST API ##
##############

resource "azurerm_api_management_api_version_set" "api_pn_integration_api" {
  name                = format("%s-pn-integration-rest-api", var.env_short)
  api_management_name = local.apim.name
  resource_group_name = local.apim.rg
  display_name        = "GPD PN Integration"
  versioning_scheme   = "Segment"
}

module "apim_api_pn_integration_gpd_api_v1" {
  source = "git::https://github.com/pagopa/terraform-azurerm-v3.git//api_management_api?ref=v8.62.1"

  name                  = format("%s-pn-integration-gpd-api-aks", var.env_short)
  api_management_name = local.apim.name
  resource_group_name = local.apim.rg
  product_ids           = [local.apim.pn_integration_product_id]
  subscription_required = true
  version_set_id        = azurerm_api_management_api_version_set.api_pn_integration_api.id
  api_version           = "v1"

  description  = "GPD API per Piattaforma Notifiche"
  display_name = "GPD PN Integration"
  path         = local.apim_pn_integration_rest_api.gpd_service.path
  protocols    = ["https"]
  service_url  = local.gpd_core_service_url

  content_format = "openapi"

  content_value = templatefile("../../openapi/openapi_send_v1.json", {
    service = local.apim_pn_integration_rest_api.gpd_service.path
  })

  xml_content = file("./api/pn_integration_api/v1/_base_policy${var.env_short != "p" ? "_mock" : ""}" {
  })
}