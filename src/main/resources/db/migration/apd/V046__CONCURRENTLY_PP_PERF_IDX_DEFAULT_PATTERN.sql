-- Optimizes the most common query pattern on payment_position: filtering by organization_fiscal_code, excluding WISP, and ordering 
-- by inserted_date (default pagination).
-- The exclusion of WISP reflects the application-level behavior: production statistics show that serviceType is null for the vast majority of requests, 
-- this partial index excludes WISP rows by design, reducing index size and improving selectivity, ordering, and pagination performance 
-- for the dominant workload.
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_pp_org_insdate_notwisp
ON apd.payment_position (organization_fiscal_code, inserted_date)
WHERE service_type <> 'WISP';