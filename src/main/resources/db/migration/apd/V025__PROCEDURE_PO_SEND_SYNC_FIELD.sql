-- Step 2: we update the column in batches: this avoids long transactions and keeps write locks short.
CREATE OR REPLACE PROCEDURE apd.update_payment_option_send_sync(IN batch_size integer)
 LANGUAGE plpgsql
AS $procedure$
DECLARE
  updated_rows INTEGER := 0;
BEGIN
  LOOP
    WITH to_update AS (
      SELECT id
      FROM apd.payment_option
      WHERE send_sync IS NULL
      LIMIT batch_size
    )
    UPDATE apd.payment_option
    SET send_sync = false
    WHERE id IN (SELECT id FROM to_update);

    GET DIAGNOSTICS updated_rows = ROW_COUNT;
    EXIT WHEN updated_rows = 0;
    PERFORM pg_sleep(0.1); -- short pause between batches
  END LOOP;
END;
$procedure$
;

CALL update_payment_option_send_sync(1000)