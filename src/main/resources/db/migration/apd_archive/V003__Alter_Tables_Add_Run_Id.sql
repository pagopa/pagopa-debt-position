ALTER TABLE apd.payment_position
ADD COLUMN IF NOT EXISTS migration_run_id text;

ALTER TABLE apd.payment_option
ADD COLUMN IF NOT EXISTS migration_run_id text;

ALTER TABLE apd.payment_option_metadata
ADD COLUMN IF NOT EXISTS migration_run_id text;

ALTER TABLE apd.transfer
ADD COLUMN IF NOT EXISTS migration_run_id text;

ALTER TABLE apd.transfer_metadata
ADD COLUMN IF NOT EXISTS migration_run_id text;