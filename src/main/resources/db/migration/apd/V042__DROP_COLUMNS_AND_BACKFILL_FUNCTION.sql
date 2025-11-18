-- ============================================================================
-- Drop validity_date / switch_to_expired columns and backfill functions
-- ============================================================================

-- 1) Removing columns from payment_position
ALTER TABLE payment_position DROP COLUMN IF EXISTS validity_date;
ALTER TABLE payment_position DROP COLUMN IF EXISTS switch_to_expired;

-- 2) Backfill function removal
DROP FUNCTION IF EXISTS fn_backfill_payment_plan_id_single_option(integer);
DROP FUNCTION IF EXISTS fn_backfill_payment_plan_id_grouped_batch(integer);
DROP FUNCTION IF EXISTS fn_backfill_switch_to_expired(integer);
DROP FUNCTION IF EXISTS fn_backfill_validity_date(integer);