START TRANSACTION;
ALTER TABLE transfer DROP CONSTRAINT uniquetransfer;
ALTER TABLE transfer ADD CONSTRAINT unique_transfer UNIQUE(iuv, organization_fiscal_code, transfer_id, payment_option_id);
COMMIT;