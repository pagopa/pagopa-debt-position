ALTER TABLE apd.payment_position
ADD COLUMN IF NOT EXISTS migration_run_id text;