-- =========================
-- PO UPDATE
-- =========================

ALTER TABLE payment_option ADD COLUMN IF NOT EXISTS payment_option_description TEXT;