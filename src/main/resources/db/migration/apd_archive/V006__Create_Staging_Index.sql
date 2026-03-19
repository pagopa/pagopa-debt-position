-- Index to speed up reading and deletion by the Stored Procedure
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_archiving_staging_run_id
ON apd.archiving_staging (run_id);