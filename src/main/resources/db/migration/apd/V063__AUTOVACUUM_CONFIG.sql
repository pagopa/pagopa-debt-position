/*
 * We apply autovacuum scale factors (1%) strictly at the table level
 * rather than globally in postgresql.conf.
 *
 * Rationale: A global 0.01 setting would trigger constant, unnecessary vacuum cycles
 * on smaller, frequently updated tables. This wastes CPU/IO resources and leads to
 * "Autovacuum worker starvation", where all available background workers are busy
 * cleaning tiny tables, leaving massive tables unattended and prone to severe bloat.
 * Targeting only these specific large tables ensures timely maintenance without
 * degrading overall database performance.
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