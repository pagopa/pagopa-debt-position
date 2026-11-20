prefix          = "pagopa"
env_short       = "d"
env             = "dev"
domain          = "pos"
location        = "westeurope"
location_short  = "weu"
location_string = "West Europe"
instance        = "dev"

tags = {
  CreatedBy   = "Terraform"
  Environment = "Dev"
  Owner       = "pagoPA"
  Source      = "https://github.com/pagopa/pagopa-debt-position"
  CostCenter  = "TS310 - PAGAMENTI & SERVIZI"
}

apim_dns_zone_prefix = "dev.platform"
external_domain      = "pagopa.it"
hostname             = "weudev.pos.internal.dev.platform.pagopa.it"
aca_hostname = ""