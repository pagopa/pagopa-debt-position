-- Step 1: we add the column as nullable. This way PostgreSQL does not physically update each row right away.
ALTER TABLE payment_option ADD send_sync boolean DEFAULT false