DROP FUNCTION IF EXISTS apd.fn_backfill_switch_to_expired(INT);

-- ZDT: lock, update, count, and release locks in a single atomic operation
CREATE OR REPLACE FUNCTION apd.fn_backfill_switch_to_expired(
    batch_size INT
)
RETURNS INT -- Returns the number of rows updated
LANGUAGE plpgsql
AS $$
DECLARE
    updated_rows INT;
BEGIN
    -- 1. Find the batch, update and count
    WITH batch_to_update AS (
        SELECT po.id
        FROM apd.payment_option po
        WHERE po.switch_to_expired IS NULL
        ORDER BY po.id
        LIMIT batch_size
        FOR UPDATE SKIP LOCKED
    ),
    update_result AS (
        UPDATE apd.payment_option po
        SET switch_to_expired = pp.switch_to_expired
        FROM
            apd.payment_position pp,
            batch_to_update btu
        WHERE
            po.id = btu.id
            AND po.payment_position_id = pp.id
        RETURNING 1 -- Returns a value for each updated row
    )
    -- 2. Counts the rows that have actually been updated
    SELECT count(*) INTO updated_rows FROM update_result;

    -- 3. Returns the count to the caller
    RETURN updated_rows;
END;
$$;
