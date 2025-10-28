-- =========================
-- PO UPDATE
-- =========================

DROP INDEX IF EXISTS payment_option_payment_position_id_idx;
DROP INDEX IF EXISTS payment_position_status_validity_date_idx;
DROP INDEX IF EXISTS idx_status_validity_date;


ALTER TABLE payment_option ADD COLUMN IF NOT EXISTS payment_plan_id TEXT;
ALTER TABLE payment_option ADD COLUMN IF NOT EXISTS switch_to_expired boolean;
ALTER TABLE payment_option ADD COLUMN IF NOT EXISTS validity_date timestamp;

CREATE INDEX IF NOT EXISTS idx_status_validity_date ON payment_option (status, validity_date);