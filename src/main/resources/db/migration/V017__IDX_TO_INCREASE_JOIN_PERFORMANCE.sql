CREATE INDEX IF NOT EXISTS idx_payment_position_id ON payment_option (payment_position_id);
CREATE INDEX IF NOT EXISTS idx_transfer_payment_option_id ON transfer (payment_option_id);
CREATE INDEX IF NOT EXISTS idx_payment_option_metadata_payment_option_id ON payment_option_metadata (payment_option_id);
CREATE INDEX IF NOT EXISTS idx_transfer_id ON transfer_metadata (transfer_id);