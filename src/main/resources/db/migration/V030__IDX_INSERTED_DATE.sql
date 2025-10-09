CREATE INDEX IF NOT EXISTS idx_payment_option_inserted_date
ON payment_option (inserted_date);

CREATE INDEX IF NOT EXISTS idx_payment_position_inserted_date
ON payment_position (inserted_date);

CREATE INDEX IF NOT EXISTS idx_transfer_inserted_date
ON transfer (inserted_date);