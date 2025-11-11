-- ===========================================================================
-- Create idx_status_validity_date.
-- ===========================================================================

CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_status_validity_date ON payment_option (status, validity_date);