prefix          = "pagopa"
env_short       = "u"
env             = "uat"
domain          = "aca"
location        = "westeurope"
location_short  = "weu"
location_string = "West Europe"
instance        = "uat"

tags = {
  CreatedBy   = "Terraform"
  Environment = "Uat"
  Owner       = "pagoPA"
  Source      = "https://github.com/pagopa/pagopa-debt-position"
  CostCenter  = "TS310 - PAGAMENTI & SERVIZI"
}

apim_dns_zone_prefix = "uat.platform"
external_domain      = "pagopa.it"
hostname             = "weuuat.aca.internal.uat.platform.pagopa.it"
