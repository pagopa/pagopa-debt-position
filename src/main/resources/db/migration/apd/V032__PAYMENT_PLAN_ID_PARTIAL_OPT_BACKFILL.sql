DROP PROCEDURE IF EXISTS apd.backfill_payment_plan_id_grouped_batch(INT, NUMERIC);

CREATE OR REPLACE PROCEDURE apd.backfill_payment_plan_id_grouped_batch(
    batch_size INT DEFAULT 1000,
    sleep_seconds NUMERIC DEFAULT 0.1
)
LANGUAGE plpgsql
AS $$
DECLARE
    rows_updated INT;
    total_rows BIGINT := 0;
    target_table TEXT := 'apd.payment_option';
    query TEXT;
BEGIN
    RAISE NOTICE 'Starting batch migration of payment_plan_id PARTIAL_OPTION...';
    LOOP
        -- We use EXECUTE to parameterize the schema.
        query := format(
            '
            -- 1. Find a batch of groups (payment_position_id) to update.
            --    To do this safely, we find a batch of *rows* (using SKIP LOCKED)
            --    and then take their unique payment_position_ids.
            WITH rows_to_find_groups AS (
                SELECT id, payment_position_id
                FROM %1$s
                WHERE payment_plan_id IS NULL
                  AND is_partial_payment IS TRUE
                LIMIT %2$s
                FOR UPDATE SKIP LOCKED
            ),
            -- 2. Extract the unique groups from this batch of rows
            groups_in_batch AS (
                SELECT DISTINCT payment_position_id
                FROM rows_to_find_groups
            ),
            -- 3. Generate a UUID for each group
            group_uuids AS (
                SELECT
                    payment_position_id,
                    gen_random_uuid()::text AS u
                FROM groups_in_batch
            )
            -- 4. UPDATE *all* rows for the groups found
            UPDATE %1$s po
            SET payment_plan_id = gu.u
            FROM group_uuids gu
            WHERE po.payment_position_id = gu.payment_position_id
              AND po.payment_plan_id IS NULL -- Safety condition
              AND po.is_partial_payment IS TRUE; -- Safety condition
            ',
            target_table,         -- %1$s
            batch_size       -- %2$s
        );

        EXECUTE query;

        GET DIAGNOSTICS rows_updated = ROW_COUNT;

        -- 5. Exit if we haven't updated anything else.
        EXIT WHEN rows_updated = 0;

        total_rows := total_rows + rows_updated;
        RAISE NOTICE '% rows updated in this batch (Total: %)', rows_updated, total_rows;

        -- 6. Save the batch job
        COMMIT;

        -- 7. Short pause to reduce I/O load.
        PERFORM pg_sleep(sleep_seconds);

    END LOOP;
    RAISE NOTICE 'Batch migration completed. Total rows updated: %', total_rows;
END;
$$;
