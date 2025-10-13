-- 1 backfill from payment_position_id
UPDATE installment i
SET switch_to_expired = pp.switch_to_expired
FROM payment_position pp
WHERE i.payment_position_id = pp.id;

-- 2 for safety: any NULLs -> false
UPDATE installment
SET switch_to_expired = false
WHERE switch_to_expired IS NULL;

-- 3 NOT NULL + default
ALTER TABLE installment
  ALTER COLUMN switch_to_expired SET NOT NULL,
  ALTER COLUMN switch_to_expired SET DEFAULT false;

-- 4 indice composito per velocizzare existsByPaymentPosition_IdAndSwitchToExpiredTrue (capire se ha senso aggiungerlo)  
--CREATE INDEX IF NOT EXISTS idx_installment_ppid_switch_expired
--  ON installment(payment_position_id, switch_to_expired)
--  WHERE switch_to_expired = true;
  
-- 5 drop from payment_position
--ALTER TABLE payment_position
--  DROP COLUMN IF EXISTS switch_to_expired;