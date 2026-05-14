-- -------------------------------------------------------------------------
-- Payment positions eligible for EXPIRED evaluation:
-- only VALID positions with no payment date.
-- Used when joining candidate payment options to payment_position.
-- -------------------------------------------------------------------------
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_pp_valid_payment_date_null_id
ON apd.payment_position (id)
WHERE status = 'VALID'
  AND payment_date IS NULL;


-- -------------------------------------------------------------------------
-- Fast check for the rule:
-- "all payment options must be PO_UNPAID".
-- The NOT EXISTS condition searches for any PO with status <> PO_UNPAID.
-- -------------------------------------------------------------------------
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_po_ppid_status_not_unpaid
ON apd.payment_option (payment_position_id)
WHERE status <> 'PO_UNPAID';


-- -------------------------------------------------------------------------
-- Fast check for the rule:
-- "all payment options must be expired".
-- The NOT EXISTS condition searches for any PO with due_date >= currentDate.
-- -------------------------------------------------------------------------
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_po_ppid_due_date
ON apd.payment_option (payment_position_id, due_date)
WHERE due_date IS NOT NULL;


-- -------------------------------------------------------------------------
-- Fast check for the rule:
-- "all payment options must have switch_to_expired = true".
-- NULL is treated as not eligible, therefore IS DISTINCT FROM TRUE is used.
-- -------------------------------------------------------------------------
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_po_ppid_switch_to_expired_not_true
ON apd.payment_option (payment_position_id)
WHERE switch_to_expired IS DISTINCT FROM TRUE;


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


-- -------------------------------------------------------------------------
-- Refresh statistics after index changes.
-- -------------------------------------------------------------------------
ANALYZE apd.payment_option;
ANALYZE apd.payment_position;