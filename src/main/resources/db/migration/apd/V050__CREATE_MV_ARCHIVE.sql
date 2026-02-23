-- Creating the Materialized View
-- Extract only the ID and date to keep the view lightweight.
CREATE MATERIALIZED VIEW apd.archiving_selection_buffer AS
SELECT
    id,
    last_updated_date,
    status,                             -- audits and quick filters
FROM
    apd.payment_position
WHERE
    status IN ('PAID', 'EXPIRED', 'REPORTED', 'INVALID')
    AND last_updated_date < (CURRENT_DATE - INTERVAL '2 years')
WITH DATA;