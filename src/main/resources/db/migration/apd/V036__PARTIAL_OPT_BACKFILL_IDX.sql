-- This migration creates 4 indexes to improve backfill performance;
-- As the backfill progresses, rows (that meet the condition) become increasingly rare.
-- To find the next batch, the database is forced to scan the po.id index, skipping millions of rows that have already been updated,
-- just to find the few remaining NULL rows. This process becomes exponentially slower towards the end of the backfill.

-- 2. Index for: fn_backfill_payment_plan_id_grouped_batch
CREATE INDEX CONCURRENTLY idx_payment_option_grouped_pending
ON apd.payment_option (id)
WHERE payment_plan_id IS NULL AND is_partial_payment IS TRUE;