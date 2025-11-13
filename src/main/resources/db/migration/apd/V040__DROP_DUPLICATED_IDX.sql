DROP INDEX CONCURRENTLY IF EXISTS payment_option_payment_position_id_idx;
-- Drop index when pp.validity_date will no longer be used
-- DROP INDEX IF EXISTS payment_position_status_validity_date_idx;
-- DROP INDEX IF EXISTS idx_status_validity_date;