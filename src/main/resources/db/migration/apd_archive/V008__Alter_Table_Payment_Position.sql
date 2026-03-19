ALTER TABLE apd.payment_position
ADD COLUMN IF NOT EXISTS archiving_timestamp timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL;