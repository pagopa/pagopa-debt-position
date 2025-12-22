-- ============================================================================
-- Drop backfill functions
-- ============================================================================

DROP FUNCTION IF EXISTS fn_backfill_payment_plan_id_single_option(integer);
DROP FUNCTION IF EXISTS fn_backfill_payment_plan_id_grouped_batch(integer);
DROP FUNCTION IF EXISTS fn_backfill_switch_to_expired(integer);
DROP FUNCTION IF EXISTS fn_backfill_validity_date(integer);