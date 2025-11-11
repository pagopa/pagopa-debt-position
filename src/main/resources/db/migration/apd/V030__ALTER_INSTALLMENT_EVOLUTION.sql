-- =========================
-- PO UPDATE
-- =========================

ALTER TABLE payment_option ADD COLUMN IF NOT EXISTS payment_plan_id TEXT;
ALTER TABLE payment_option ADD COLUMN IF NOT EXISTS switch_to_expired boolean;
ALTER TABLE payment_option ADD COLUMN IF NOT EXISTS validity_date timestamp;