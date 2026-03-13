-- Index to speed up housekeeping jobs (e.g., deleting old records in staging table)
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_archiving_staging_ingestion
ON apd.archiving_staging (ingestion_timestamp);