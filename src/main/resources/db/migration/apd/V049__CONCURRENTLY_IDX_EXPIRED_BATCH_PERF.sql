-- -------------------------------------------------------------------------
-- Fast check for the rule:
-- "all payment options must be PO_UNPAID".
-- The NOT EXISTS condition searches for any PO with status <> PO_UNPAID.
-- -------------------------------------------------------------------------
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_po_ppid_status_not_unpaid
ON apd.payment_option (payment_position_id)
WHERE status <> 'PO_UNPAID';

-- -------------------------------------------------------------------------
-- Main driving index for the optimized EXPIRED batch query.
-- It starts from already expired, unpaid, switchable payment options instead
-- of scanning all VALID payment positions.
-- -------------------------------------------------------------------------
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_po_expired_candidate
ON apd.payment_option (due_date, payment_position_id)
WHERE status = 'PO_UNPAID'
  AND switch_to_expired IS TRUE
  AND due_date IS NOT NULL;