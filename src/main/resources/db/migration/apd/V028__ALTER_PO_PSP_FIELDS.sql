-- idPSP https://github.com/pagopa/pagopa-api/blob/c752179c66da9e3a2a71dd16397fde6b0ad08818/wsdl/xsd/paForNode.xsd#L219 stText35
-- pspFiscalCode https://github.com/pagopa/pagopa-api/blob/c752179c66da9e3a2a71dd16397fde6b0ad08818/wsdl/xsd/paForNode.xsd#L220 stText70
-- pspPartitaIVA https://github.com/pagopa/pagopa-api/blob/c752179c66da9e3a2a71dd16397fde6b0ad08818/wsdl/xsd/paForNode.xsd#L221 stText20
ALTER TABLE payment_option ADD COLUMN IF NOT EXISTS "psp_code" varchar(50) NULL;
ALTER TABLE payment_option ADD COLUMN IF NOT EXISTS "psp_tax_code" varchar(70) NULL;