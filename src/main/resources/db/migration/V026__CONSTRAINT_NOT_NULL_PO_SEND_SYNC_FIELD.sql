-- Step 3: after updates, we add NOT NULL constraint
ALTER TABLE payment_option ALTER COLUMN send_sync SET NOT NULL;

DROP PROCEDURE IF EXISTS update_payment_option_send_sync;