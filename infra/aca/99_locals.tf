locals {
  product = "${var.prefix}-${var.env_short}"
  project = "${var.prefix}-${var.env_short}-${var.location_short}-${var.domain}"

  apim = {
    name       = "${local.product}-apim"
    rg         = "${local.product}-api-rg"
    aca_product_id = "aca"
  }

  gpd_core_service_url        = var.env == "prod" ? "https://weu${var.env}.gps.internal.platform.pagopa.it/pagopa-gpd-core" : "https://weu${var.env}.gps.internal.${var.env}.platform.pagopa.it/pagopa-gpd-core"
}