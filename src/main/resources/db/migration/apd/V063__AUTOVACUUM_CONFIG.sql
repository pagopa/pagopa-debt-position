/*
 * We apply autovacuum scale factors (2%) strictly at the table level
 * rather than globally in postgresql.conf.
 */

ALTER TABLE apd.payment_position SET (
    autovacuum_vacuum_scale_factor = 0.02,
    autovacuum_analyze_scale_factor = 0.02
);

ALTER TABLE apd.payment_option SET (
    autovacuum_vacuum_scale_factor = 0.02,
    autovacuum_analyze_scale_factor = 0.02
);

ALTER TABLE apd.transfer SET (
    autovacuum_vacuum_scale_factor = 0.02,
    autovacuum_analyze_scale_factor = 0.02
);