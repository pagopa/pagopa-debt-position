-- Prevents expensive nested-loop scans in rare but very costly due_date queries by allowing fast index lookups per payment_position
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_po_org_ppid_duedate
ON apd.payment_option (organization_fiscal_code, payment_position_id, due_date);