-- Unique Index on MV (Required for REFRESH CONCURRENTLY)
CREATE UNIQUE INDEX CONCURRENTLY IF NOT EXISTS idx_archiving_sel_buf_pk ON apd.archiving_selection_buffer (id);