locals {
  product = "${var.prefix}-${var.env_short}"
  project = "${var.prefix}-${var.env_short}-${var.location_short}-${var.domain}"

  apim = {
    name       = "${local.product}-apim"
    rg         = "${local.product}-api-rg"
    internal_gpd_product_id = "product-gpd"
    external_gpd_product_id = "debt-positions"
    gpd_integration_product_id = "debt-positions-integration"
    aca_integration_product_id = "aca-integration"
    pn_integration_product_id = "pn-integration"
  }

  gpd_core_service_url        = var.env == "prod" ? "https://weu${var.env}.gps.internal.platform.pagopa.it/pagopa-gpd-core" : "https://weu${var.env}.gps.internal.${var.env}.platform.pagopa.it/pagopa-gpd-core"

  external = {
    display_name          = "GPD pagoPA - Debt Positions service API for organizations"
    description           = "API to support Debt Positions service for organizations"
    path                  = "gpd/debt-positions-service"
    subscription_required = true
  }
}