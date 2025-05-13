ALTER TABLE payment_option 
    ADD COLUMN IF NOT EXISTS payment_position_status VARCHAR(55) NULL;