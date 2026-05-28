-- This index is used to add information to the WAL via REPLICA IDENTITY USING INDEX.
CREATE UNIQUE INDEX CONCURRENTLY IF NOT EXISTS idx_tf_replica_archived ON apd.transfer (id, archived);