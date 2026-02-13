-- Optimizes the most common query pattern on payment_position: filtering by organization_fiscal_code, excluding WISP, and ordering 
-- by inserted_date (default pagination).
-- The exclusion of WISP reflects the application-level behavior: production statistics show that serviceType is null for the vast majority of requests, 
-- this partial index excludes WISP rows by design, reducing index size and improving selectivity, ordering, and pagination performance 
-- for the dominant workload.
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_pp_org_insdate_notwisp
ON apd.payment_position (organization_fiscal_code, inserted_date)
WHERE service_type <> 'WISP';

-- Enables index-only scans for COUNT(*), avoiding full table or parallel sequential scans.
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_pp_org_notwisp
ON apd.payment_position (organization_fiscal_code)
WHERE service_type <> 'WISP';

-- Prevents expensive nested-loop scans in rare but very costly due_date queries by allowing fast index lookups per payment_position
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_po_org_ppid_duedate
ON apd.payment_option (organization_fiscal_code, payment_position_id, due_date);