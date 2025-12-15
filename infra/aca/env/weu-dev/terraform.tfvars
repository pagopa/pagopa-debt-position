prefix          = "pagopa"
env_short       = "d"
env             = "dev"
domain          = "aca"
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
hostname             = "weudev.aca.internal.dev.platform.pagopa.it"
aca_hostname = ""