DROP PROCEDURE IF EXISTS apd.backfill_validity_date(INT, NUMERIC);

-- 1. Creation of the procedure
CREATE OR REPLACE PROCEDURE apd.backfill_validity_date(
    batch_size INT DEFAULT 1000,
    sleep_seconds NUMERIC DEFAULT 0.1
)
LANGUAGE plpgsql
AS $$
DECLARE
    updated_rows INT;
    total_updated INT := 0;
BEGIN
    RAISE NOTICE 'Start ZDT backfill for payment_option.validity_date (Batch size: %)', batch_size;

    LOOP
        -- 2. Compute the batch to update
        WITH batch_to_update AS (
			SELECT po.id
			FROM apd.payment_option po, apd.payment_position pp
			WHERE po.payment_position_id = pp.id
			  AND po.validity_date IS NULL
			  AND pp.validity_date IS NOT NULL
			ORDER BY po.id
			LIMIT batch_size
			FOR UPDATE SKIP LOCKED
        ),
        update_result AS (
            UPDATE apd.payment_option po
            SET validity_date = pp.validity_date
            FROM
                apd.payment_position pp,
                batch_to_update btu
            WHERE
                po.id = btu.id
                AND po.payment_position_id = pp.id
            RETURNING po.id
        )
        SELECT count(*) INTO updated_rows FROM update_result;

        total_updated := total_updated + updated_rows;

        -- 3. Rows commit: releases the locks and makes the changes permanent for this batch.
        COMMIT;

        -- 4. Exits the loop if the last batch was empty
        EXIT WHEN updated_rows = 0;

        -- 5. Log your progress and wait
        RAISE NOTICE '% rows updated (Total: %). Pause for %s...', updated_rows, total_updated, sleep_seconds;
        PERFORM pg_sleep(sleep_seconds);

    END LOOP;

    RAISE NOTICE 'ZDT backfill completed. Total rows updated: %', total_updated;
END $$;
