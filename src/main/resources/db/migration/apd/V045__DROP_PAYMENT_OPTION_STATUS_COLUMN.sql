-- Remove denormalized payment_position_status column from payment_option
ALTER TABLE payment_option 
DROP COLUMN IF EXISTS payment_position_status;