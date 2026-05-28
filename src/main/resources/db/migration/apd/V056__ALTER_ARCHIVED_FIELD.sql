-- The archived column stores whether a record has been archived or not.
ALTER TABLE apd.payment_position ADD COLUMN IF NOT EXISTS archived BOOLEAN NOT NULL DEFAULT FALSE;
ALTER TABLE apd.payment_option ADD COLUMN IF NOT EXISTS archived BOOLEAN NOT NULL DEFAULT FALSE;
ALTER TABLE apd.transfer ADD COLUMN IF NOT EXISTS archived BOOLEAN NOT NULL DEFAULT FALSE;