locals {
  product = "${var.prefix}-${var.env_short}"
  project = "${var.prefix}-${var.env_short}-${var.location_short}-${var.domain}"

  apim = {
    name           = "${local.product}-apim"
    rg             = "${local.product}-api-rg"
    pos_product_id = "pos-integration"
  }

  kv = {
    name = "${local.product}-gps-kv"
    rg   = "${local.product}-gps-sec-rg"
  }

  apim_hostname        = var.env == "prod" ? "api.platform.pagopa.it" : "api.${var.env}.platform.pagopa.it"
  gpd_core_service_url = var.env == "prod" ? "https://weu${var.env}.gps.internal.platform.pagopa.it/pagopa-gpd-core" : "https://weu${var.env}.gps.internal.${var.env}.platform.pagopa.it/pagopa-gpd-core"
}