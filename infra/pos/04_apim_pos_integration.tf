#############################
## Product pos-integration ##
#############################

resource "azurerm_api_management_product" "apim_pos_integration_product" {
  product_id            = local.apim.pos_product_id
  display_name          = "POS integration"
  description           = "Integrazione POS per Gestione Posizioni Debitorie"
  resource_group_name   = local.apim.rg
  api_management_name   = local.apim.name
  published             = false
  subscription_required = true
  approval_required     = true
  subscriptions_limit   = 100
}

####################
## Local variables #
####################

locals {
  apim_pos_integration_api = {
    display_name          = "GPD for POS pagoPA - Debt Positions service API for POS"
    description           = "API per integrazione POS con Gestione Posizioni Debitorie (GPD)"
    path                  = "pos-integration-gpd/api"
    subscription_required = true
  }
}

#######################
## API Version Set   ##
#######################

resource "azurerm_api_management_api_version_set" "api_pos_integration" {
  name                = format("%s-gpd-pos-integration-api", var.env_short)
  api_management_name = local.apim.name
  resource_group_name = local.apim.rg
  display_name        = local.apim_pos_integration_api.display_name
  versioning_scheme   = "Segment"
}

#######################
## API Management V3 ##
#######################

resource "azurerm_api_management_api" "apim_pos_integration_api_v3" {
  name                  = format("%s-gpd-pos-integration-api-v3", var.env_short)
  api_management_name   = local.apim.name
  resource_group_name   = local.apim.rg
  subscription_required = local.apim_pos_integration_api.subscription_required
  version_set_id        = azurerm_api_management_api_version_set.api_pos_integration.id
  version               = "v3"
  revision              = "1"

  description  = local.apim_pos_integration_api.description
  display_name = "${local.apim_pos_integration_api.display_name} - V3"

  path        = local.apim_pos_integration_api.path
  protocols   = ["https"]
  service_url = "${local.gpd_core_service_url}/v3"

  import {
    content_format = "openapi+json"
    content_value = templatefile("../../openapi/openapi_pos_v3.json", {
      host    = local.apim_hostname
      service = "gpd"
    })
  }
}

##################################
## Association with POS Product ##
##################################

resource "azurerm_api_management_product_api" "apim_pos_integration_product_api_v3" {
  api_management_name = local.apim.name
  resource_group_name = local.apim.rg

  product_id = azurerm_api_management_product.apim_pos_integration_product.product_id
  api_name   = azurerm_api_management_api.apim_pos_integration_api_v3.name
}

#######################
## API Policy        ##
#######################

resource "azurerm_api_management_api_policy" "apim_pos_integration_policy_v3" {
  api_name            = azurerm_api_management_api.apim_pos_integration_api_v3.name
  api_management_name = local.apim.name
  resource_group_name = local.apim.rg

  xml_content = templatefile("./api/pos_api/v3/_base_policy.xml", {})
}

####################################
## Subscription for POS Integration
####################################

resource "azurerm_api_management_subscription" "pos_integration_subscription" {
  api_management_name = local.apim.name
  resource_group_name = local.apim.rg
  product_id          = azurerm_api_management_product.apim_pos_integration_product.id
  display_name        = format("%s - Subscription for POS Integration", upper(var.env_short))
  allow_tracing       = false
  state               = "active"
}

####################################
## Key Valut for POS Integration
####################################

data "azurerm_key_vault" "kv" {
  name                = local.kv.name
  resource_group_name = local.kv.rg
}


resource "azurerm_key_vault_secret" "pos_integration_subscription_key" {
  name         = "pos-integration-gpd-subscription-key"
  value        = azurerm_api_management_subscription.pos_integration_subscription.primary_key
  key_vault_id = data.azurerm_key_vault.kv.id
}