-- ============================================================================
-- Drop indexes with CONCURRENTLY mode
-- ============================================================================

-- 1) Index on payment_position
DROP INDEX CONCURRENTLY IF EXISTS payment_position_status_validity_date_idx;

-- 2) Indexes on payment_option
DROP INDEX CONCURRENTLY IF EXISTS idx_payment_option_single_pending;
DROP INDEX CONCURRENTLY IF EXISTS idx_payment_option_grouped_pending;
DROP INDEX CONCURRENTLY IF EXISTS idx_payment_option_switch_expired_pending;
DROP INDEX CONCURRENTLY IF EXISTS idx_payment_option_validity_date_pending;
