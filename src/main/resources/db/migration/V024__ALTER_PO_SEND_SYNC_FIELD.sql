-- Step 1: we add the column as nullable. This way PostgreSQL does not physically update each row right away.
ALTER TABLE payment_option ADD send_sync boolean DEFAULT false

-- Step 2: we update the column in batches: this avoids long transactions and keeps write locks short.
CREATE PROCEDURE update_payment_option_send_sync(batch_size INT)
LANGUAGE SQL
AS $$
    DECLARE
      updated_rows INTEGER := 0;
    BEGIN
      LOOP
        UPDATE payment_option po
        SET send_sync = true
        WHERE po.send_sync IS NULL
        LIMIT batch_size;

        GET DIAGNOSTICS updated_rows = ROW_COUNT;
        EXIT WHEN updated_rows = 0;

        PERFORM pg_sleep(0.1); -- short pause between batches
      END LOOP;
    END $$;

CALL update_payment_option_send_sync(100)

-- Step 3: after updates, we add NOT NULL constraint
ALTER TABLE payment_option ALTER COLUMN send_sync SET NOT NULL;