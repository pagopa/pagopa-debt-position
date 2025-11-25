prefix                 = "pagopa"
env_short              = "p"
env                    = "prod"
domain                 = "gps"
location               = "westeurope"
location_short         = "weu"
location_string        = "West Europe"
instance               = "prod"

tags = {
  CreatedBy   = "Terraform"
  Environment = "Prod"
  Owner       = "pagoPA"
  Source      = "https://github.com/pagopa/pagopa-debt-position"
  CostCenter  = "TS310 - PAGAMENTI & SERVIZI"
}

apim_dns_zone_prefix = "platform"
external_domain      = "pagopa.it"
hostname             = "weuprod.gps.internal.platform.pagopa.it"
