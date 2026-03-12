CREATE OR REPLACE PROCEDURE apd.refresh_archiving_buffer()
LANGUAGE plpgsql
AS $$
BEGIN
    -- Increase the memory for this session to speed up sort/diff
    SET LOCAL maintenance_work_mem = '1GB';

    RAISE NOTICE 'Starting concurrent refresh of archiving_selection_buffer...';

    REFRESH MATERIALIZED VIEW CONCURRENTLY apd.archiving_selection_buffer;

    -- ANALYZE to update statistics for the ADF optimizer
    ANALYZE apd.archiving_selection_buffer;

    RAISE NOTICE 'Refresh completed successfully.';
END;
$$;