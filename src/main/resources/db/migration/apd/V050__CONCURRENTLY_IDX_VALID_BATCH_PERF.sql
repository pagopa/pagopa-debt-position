-- -------------------------------------------------------------------------
-- Supports the PUBLISHED -> VALID batch by quickly finding only published payment positions.
-- -------------------------------------------------------------------------
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_pp_published_id
ON apd.payment_position (id)
WHERE status = 'PUBLISHED';

-- -------------------------------------------------------------------------
-- Refresh statistics after index changes.
-- -------------------------------------------------------------------------
ANALYZE apd.payment_option;
ANALYZE apd.payment_position;