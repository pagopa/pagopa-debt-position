-- This migration creates 2 indexes to improve backfill performance;
-- As the backfill progresses, rows with switch_to_expired (or validity_date) IS NULL become increasingly rare.
-- To find the next batch, the database is forced to scan the po.id index, skipping millions of rows that have already been updated,
-- just to find the few remaining NULL rows. This process becomes exponentially slower towards the end of the backfill.

DROP INDEX CONCURRENTLY IF EXISTS apd.idx_payment_option_backfill_pending;

CREATE INDEX CONCURRENTLY idx_payment_option_backfill_pending
ON apd.payment_option (id)
WHERE switch_to_expired IS NULL;

DROP INDEX CONCURRENTLY IF EXISTS apd.idx_payment_option_validity_date_pending;

CREATE INDEX CONCURRENTLY idx_payment_option_validity_date_pending
ON apd.payment_option (id)
WHERE validity_date IS NULL;