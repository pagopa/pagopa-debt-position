<<<<<<< HEAD
DROP PROCEDURE IF EXISTS backfill_payment_option(integer, interval);

CREATE PROCEDURE backfill_payment_option(
    IN p_batch_size  integer  DEFAULT 10000,                -- batch size
    IN p_lock_pause  interval DEFAULT '100 milliseconds'    -- pause between batches
)
LANGUAGE plpgsql
AS $$
DECLARE
    v_rows   integer;
    v_single text := 'SINGLE_OPTION';
BEGIN
    -- Avoid aborts on slow batches
    PERFORM set_config('statement_timeout', '0', true);

    -- ===========
    -- Temp Table: 1 UUID for payment_position (for installment plans)
    -- ===========
    CREATE TEMP TABLE IF NOT EXISTS tmp_uuid_per_position (
        payment_position_id bigint PRIMARY KEY,
        u text NOT NULL
    ) ON COMMIT PRESERVE ROWS;

    TRUNCATE tmp_uuid_per_position;

    INSERT INTO tmp_uuid_per_position (payment_position_id, u)
    SELECT payment_position_id, gen_random_uuid()::text
    FROM payment_option
    WHERE is_partial_payment IS TRUE
    GROUP BY payment_position_id;

    CREATE INDEX IF NOT EXISTS tmp_uuid_per_position_pos_idx
      ON tmp_uuid_per_position (payment_position_id);

    -- =========================
    -- Phase A: Single option => 'SINGLE_OPTION'
    -- =========================
    LOOP
        WITH to_fix AS (
            SELECT id
            FROM payment_option
            WHERE is_partial_payment IS NOT TRUE
              AND (payment_plan_id IS DISTINCT FROM v_single)
            ORDER BY id
            FOR UPDATE SKIP LOCKED
            LIMIT p_batch_size
        )
        UPDATE payment_option p
        SET payment_plan_id = v_single
        WHERE p.id IN (SELECT id FROM to_fix);

        GET DIAGNOSTICS v_rows = ROW_COUNT;
        EXIT WHEN v_rows = 0;

        PERFORM pg_sleep(EXTRACT(EPOCH FROM p_lock_pause));
    END LOOP;

    -- =========================
    -- Phase B: Installment plan => assign UUID for payment_position (only if payment_plan_id is NULL)
    -- =========================
    LOOP
        WITH batch AS (
            SELECT po.id, po.payment_position_id
            FROM payment_option po
            JOIN tmp_uuid_per_position t
              ON t.payment_position_id = po.payment_position_id
            WHERE po.is_partial_payment IS TRUE
              AND po.payment_plan_id IS NULL
            ORDER BY po.id
            FOR UPDATE OF po SKIP LOCKED
            LIMIT p_batch_size
        )
        UPDATE payment_option po
        SET payment_plan_id = t.u
        FROM batch b
        JOIN tmp_uuid_per_position t
          ON t.payment_position_id = b.payment_position_id
        WHERE po.id = b.id
          AND po.payment_plan_id IS NULL;  -- idempotente

        GET DIAGNOSTICS v_rows = ROW_COUNT;
        EXIT WHEN v_rows = 0;

        PERFORM pg_sleep(EXTRACT(EPOCH FROM p_lock_pause));
    END LOOP;

    -- Removing temporary table
    TRUNCATE tmp_uuid_per_position;
END;
$$;

CALL backfill_payment_option(p_batch_size => 10000, p_lock_pause => '100 milliseconds'::interval);

DROP PROCEDURE IF EXISTS backfill_payment_option(integer, interval);
=======
-- A) Single options: always NULL
UPDATE payment_option
SET payment_plan_id = NULL
WHERE is_partial_payment IS NOT TRUE;

-- B) payment options: one UUID for *payment_position* (only when payment_plan_id is NULL)
WITH one_uuid_per_position AS (
  SELECT
    payment_position_id,
    gen_random_uuid()::text AS u
  FROM payment_option
  WHERE is_partial_payment IS TRUE
  GROUP BY payment_position_id
)
UPDATE payment_option po
SET payment_plan_id = p.u
FROM one_uuid_per_position p
WHERE po.is_partial_payment IS TRUE
  AND po.payment_position_id = p.payment_position_id
  AND po.payment_plan_id IS NULL;
>>>>>>> branch 'odp-opt2-backfill-migration-step' of https://github.com/pagopa/pagopa-debt-position
