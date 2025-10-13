-- PO update --

ALTER TABLE IF EXISTS payment_option RENAME TO installment;
ALTER TABLE IF EXISTS installment RENAME CONSTRAINT payment_option_pkey TO installment_pkey;
ALTER TABLE IF EXISTS installment RENAME CONSTRAINT uniquepaymentopt TO uniqueinstallmentiuv;
ALTER TABLE IF EXISTS installment RENAME CONSTRAINT uniquepaymentoptnav TO uniqueinstallmentnav;

ALTER INDEX IF EXISTS idx_payment_option_inserted_date RENAME TO idx_installment_inserted_date;
ALTER INDEX IF EXISTS payment_option_pkey RENAME TO installment_pkey;
DROP INDEX CONCURRENTLY IF EXISTS payment_option_payment_position_id_idx;

ALTER SEQUENCE IF EXISTS payment_opt_seq RENAME TO installment_seq;

-- PO metadata update --

ALTER TABLE IF EXISTS payment_option_metadata RENAME TO installment_metadata;
ALTER TABLE IF EXISTS installment_metadata RENAME COLUMN payment_option_id TO installment_id;
ALTER TABLE IF EXISTS installment_metadata RENAME CONSTRAINT payment_option_metadata_pkey TO installment_metadata_pkey;
ALTER TABLE IF EXISTS installment_metadata RENAME CONSTRAINT uniquepaymentoptmetadata TO uniqueinstallmentmetadata;

DROP INDEX CONCURRENTLY IF EXISTS idx_payment_option_metadata_payment_option_id;
CREATE INDEX IF NOT EXISTS idx_installment_metadata_installment_id ON installment_metadata (installment_id);

ALTER SEQUENCE IF EXISTS payment_opt_metadata_seq RENAME TO installment_metadata_seq;