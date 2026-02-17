-- Enables index-only scans for COUNT(*), avoiding full table or parallel sequential scans.
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_pp_org_notwisp
ON apd.payment_position (organization_fiscal_code)
WHERE service_type <> 'WISP';