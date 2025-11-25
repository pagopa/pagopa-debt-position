locals {
  product = "${var.prefix}-${var.env_short}"
  project = "${var.prefix}-${var.env_short}-${var.location_short}-${var.domain}"

  apim = {
    name       = "${local.product}-apim"
    rg         = "${local.product}-api-rg"
    aca_integration_product_id = "aca-integration"
  }
  
  aca = {
    product = "${var.prefix}-${var.env_short}"
  }

  gpd_core_service_url        = var.env == "prod" ? "https://weu${var.env}.gps.internal.platform.pagopa.it/pagopa-gpd-core" : "https://weu${var.env}.gps.internal.${var.env}.platform.pagopa.it/pagopa-gpd-core"

  external = {
    display_name          = "GPD for ACA pagoPA - Debt Positions service API for ACA"
    description           = "API to support Debt Positions service for ACA"
    path                  = "aca/debt-positions-service"
    subscription_required = true
  }
}