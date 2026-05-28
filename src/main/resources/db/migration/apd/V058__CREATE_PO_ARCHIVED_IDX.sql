-- This index is used to add information to the WAL via REPLICA IDENTITY USING INDEX.
-- The info organization_fiscal_code is needed for RTP filters.
CREATE UNIQUE INDEX CONCURRENTLY IF NOT EXISTS idx_po_replica_archived ON apd.payment_option (id, organization_fiscal_code, archived);
