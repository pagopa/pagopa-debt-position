DROP PROCEDURE IF EXISTS backfill_payment_plan_id_single_option();

CREATE OR REPLACE PROCEDURE apd.backfill_payment_plan_id_single_option()
LANGUAGE plpgsql
AS $$
DECLARE
    batch_size INT := 10000; -- This parameter controls the batch size.
    rows_updated INT;
    total_rows BIGINT := 0;
BEGIN
    RAISE NOTICE 'Starting batch migration of payment_plan_id SINGLE_OPTION...';
    LOOP
        -- 1. Find a batch of target rows;
        --    Use SKIP LOCKED to prevent the application from locking.
        WITH rows_to_update AS (
            SELECT id
            FROM apd.payment_option po
            WHERE payment_plan_id IS NULL
              AND is_partial_payment IS NOT TRUE
            LIMIT batch_size
            FOR UPDATE SKIP LOCKED
        )
        -- 2. Update ONLY those rows.
        UPDATE apd.payment_option po
        SET payment_plan_id = 'SINGLE_OPTION'
        WHERE id IN (SELECT id FROM rows_to_update);

        -- 3. Check how many rows we have updated.
        GET DIAGNOSTICS rows_updated = ROW_COUNT;

        -- 4. Exit the loop if we are done.
        EXIT WHEN rows_updated = 0;

        total_rows := total_rows + rows_updated;
        RAISE NOTICE '% rows updated in this batch (Total: %)', rows_updated, total_rows;

        -- 5. Save the batch job.
        COMMIT;

        -- 6. Short pause to reduce I/O load.
        PERFORM pg_sleep(0.1); -- 100ms pause.
    END LOOP;
    RAISE NOTICE 'Batch migration completed. Total rows updated: %', total_rows;
END;
$$;

CALL apd.backfill_payment_plan_id_single_option();