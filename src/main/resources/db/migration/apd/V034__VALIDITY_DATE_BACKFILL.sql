DROP FUNCTION IF EXISTS apd.fn_backfill_validity_date(INT);

CREATE OR REPLACE FUNCTION apd.fn_backfill_validity_date(
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
        RETURNING 1 -- Returns a value for each updated row
    )
    -- 2. Counts the rows that have actually been updated
    SELECT count(*) INTO updated_rows FROM update_result;

    -- 3. Returns the count to the caller
    RETURN updated_rows;
END;
$$;
