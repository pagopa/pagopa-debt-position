ALTER TABLE apd.payment_position REPLICA IDENTITY USING INDEX idx_pp_replica_archived;
ALTER TABLE apd.payment_option REPLICA IDENTITY USING INDEX idx_po_replica_archived;
ALTER TABLE apd.transfer REPLICA IDENTITY USING INDEX idx_tf_replica_archived;