-- Creating the Materialized View
-- Extract only the ID and date to keep the view lightweight.
CREATE MATERIALIZED VIEW IF NOT EXISTS apd.archiving_selection_buffer AS
SELECT
    id,
    last_updated_date,
    status,                             -- audits and quick filters
    service_type                        -- audits and quick filters
FROM
    apd.payment_position
WHERE
    (
        service_type IN ('GPD', 'ACA')
        AND status IN ('PAID', 'REPORTED', 'INVALID') -- 'EXPIRED' is not immutable state in this case.
        AND last_updated_date < (CURRENT_DATE - INTERVAL '2 years')
    ) OR (
        service_type = 'WISP'
        AND status IN ('PAID', 'EXPIRED', 'REPORTED', 'INVALID')
        AND last_updated_date < (CURRENT_DATE - INTERVAL '6 months')
    )
WITH DATA;