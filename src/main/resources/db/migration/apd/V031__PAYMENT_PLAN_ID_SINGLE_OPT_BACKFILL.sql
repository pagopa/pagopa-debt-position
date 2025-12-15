DROP FUNCTION IF EXISTS apd.fn_backfill_payment_plan_id_single_option(INT);

-- ZDT: lock, update, count, and release locks in a single atomic operation
CREATE OR REPLACE FUNCTION apd.fn_backfill_payment_plan_id_single_option(
    batch_size INT
)
RETURNS INT -- Returns the number of rows updated
LANGUAGE plpgsql
AS $$
DECLARE
    rows_updated INT;
BEGIN
    -- 1. Find, lock (ZDT), update and count
    WITH rows_to_update AS (
        SELECT id
        FROM apd.payment_option po
        WHERE po.payment_plan_id IS NULL
          AND po.is_partial_payment IS NOT TRUE
        ORDER BY po.id
        LIMIT batch_size
        FOR UPDATE SKIP LOCKED
    ),
    update_result AS (
        UPDATE apd.payment_option po
        SET payment_plan_id = 'SINGLE_OPTION'
        FROM rows_to_update rtu
        WHERE po.id = rtu.id
        RETURNING 1 -- Returns a value for each updated row
    )
    -- 2. Count the lines that have actually been updated
    SELECT count(*) INTO rows_updated FROM update_result;

    -- 3. Return the count to the caller
    RETURN rows_updated;
END;
$$;
