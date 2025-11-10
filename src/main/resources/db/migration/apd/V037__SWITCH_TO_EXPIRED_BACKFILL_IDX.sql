-- This migration creates 4 indexes to improve backfill performance;
-- As the backfill progresses, rows (that meet the condition) become increasingly rare.
-- To find the next batch, the database is forced to scan the po.id index, skipping millions of rows that have already been updated,
-- just to find the few remaining NULL rows. This process becomes exponentially slower towards the end of the backfill.

-- 3. Index for: fn_backfill_switch_to_expired
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_payment_option_switch_expired_pending
ON apd.payment_option (id)
WHERE switch_to_expired IS NULL;