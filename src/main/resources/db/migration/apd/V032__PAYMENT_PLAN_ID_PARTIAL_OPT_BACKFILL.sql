DROP FUNCTION IF EXISTS apd.fn_backfill_payment_plan_id_grouped_batch(INT);

-- ZDT: lock, update, count, and release locks in a single atomic operation
CREATE OR REPLACE FUNCTION apd.fn_backfill_payment_plan_id_grouped_batch(
    batch_size INT
)
RETURNS INT -- Returns the number of rows updated
LANGUAGE plpgsql
AS $$
DECLARE
    rows_updated INT;
BEGIN
    WITH
    -- 1. Find a batch of rows to identify the groups to be processed
    rows_to_find_groups AS (
        SELECT id, payment_position_id
        FROM apd.payment_option po
        WHERE payment_plan_id IS NULL
          AND is_partial_payment IS TRUE
        ORDER BY po.id
        LIMIT batch_size
        FOR UPDATE SKIP LOCKED
    ),
    -- 2. Extracts unique groups from this batch of rows
    groups_in_batch AS (
        SELECT DISTINCT payment_position_id
        FROM rows_to_find_groups
    ),
    -- 3. Generates a UUID for each group
    group_uuids AS (
        SELECT
            payment_position_id,
            gen_random_uuid()::text AS u
        FROM groups_in_batch
    ),
    -- 4. UPDATE all rows for the groups found and count the modified rows
    update_result AS (
        UPDATE apd.payment_option po
        SET payment_plan_id = gu.u
        FROM group_uuids gu
        WHERE po.payment_position_id = gu.payment_position_id
          AND po.payment_plan_id IS NULL
          AND po.is_partial_payment IS TRUE
        RETURNING 1 -- Returns a value for each updated row
    )
    -- 5. Counts the lines that have actually been updated
    SELECT count(*) INTO rows_updated FROM update_result;

    -- 6. Returns the count to the caller
    RETURN rows_updated;
END;
$$;
