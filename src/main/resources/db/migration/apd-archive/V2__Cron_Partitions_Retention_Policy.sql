-- =============================================================================
-- MAINTENANCE AUTOMATION & CONFIGURABLE RETENTION
-- =============================================================================


-- Create the Expiration Procedure
CREATE OR REPLACE PROCEDURE apd.cleanup_expired_partitions()
LANGUAGE plpgsql
AS $$
DECLARE
    v_tab record;
BEGIN
    -- Look for tables that are no longer active partitions (detached by partman)
    -- and follow the established partition naming convention
    FOR v_tab IN
        SELECT n.nspname, c.relname
        FROM pg_class c
        JOIN pg_namespace n ON n.oid = c.relnamespace
        WHERE n.nspname = 'apd'
          AND c.relkind = 'r' -- Ordinary tables (physical storage, not logical partitioned parents)
          AND c.relname ~ '_p[0-9]{8}' -- Matches the _pYYYYMMDD suffix pattern
          -- Ensure the table is no longer part of any inheritance hierarchy (is detached)
          AND NOT EXISTS (
              SELECT 1 FROM pg_inherits i WHERE i.inhrelid = c.oid
          )
    LOOP
        -- Execute DROP with CASCADE to remove the table and its dependent objects (children/grandchildren)
        RAISE NOTICE 'Archiving: Dropping detached table %.% with CASCADE', v_tab.nspname, v_tab.relname;
        EXECUTE format('DROP TABLE %I.%I CASCADE', v_tab.nspname, v_tab.relname);
    END LOOP;
END;
$$;


-- Apply retention policy dynamically
UPDATE partman.part_config
SET retention = '10 years',
    retention_keep_table = true;


-- Schedule the Expiration (delete) Lifecycle Management job
SELECT cron.schedule('expiration-lifecycle-management', '0 * * * *', $JOB$
DO $$
BEGIN
    -- PHASE 1: Detach old partitions
    -- This moves partitions out of the hierarchy without breaking Foreign Keys
    PERFORM partman.run_maintenance();

    -- PHASE 2: Physical cleanup
    -- This calls the procedure to drop detached tables
    CALL apd.cleanup_expired_partitions();
END $$;
$JOB$);