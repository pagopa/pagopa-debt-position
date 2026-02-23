-- Create a partial index on the payment_position table.
-- This index allows the MV to be created and updated without scanning All the rows.
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_pp_archiving_eligibility
ON apd.payment_position (last_updated_date)
WHERE status IN ('PAID', 'EXPIRED', 'REPORTED', 'INVALID');