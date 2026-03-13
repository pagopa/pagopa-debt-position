CREATE OR REPLACE PROCEDURE apd.cleanup_staging_table(p_retention_hours INT DEFAULT 120)
LANGUAGE plpgsql
AS $$
DECLARE
    v_deleted_rows INT;
BEGIN
    -- Security check: prevents 0 or negative values from being passed.
    IF p_retention_hours <= 0 THEN
        RAISE EXCEPTION 'The p_retention_hours parameter must be greater than 0.';
    END IF;

    -- Deletion of obsolete records
    DELETE FROM apd.archiving_staging
    WHERE ingestion_timestamp < NOW() - (p_retention_hours || ' hours')::interval;

    -- Capture the number of rows deleted to write it to the logs
    GET DIAGNOSTICS v_deleted_rows = ROW_COUNT;

    RAISE NOTICE 'Housekeeping completed: % of staging records older than % hours removed.', v_deleted_rows, p_retention_hours;

END;
$$;