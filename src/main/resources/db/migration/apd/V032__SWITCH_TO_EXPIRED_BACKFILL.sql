-- 1 backfill from payment_position_id
UPDATE payment_option po
SET switch_to_expired = pp.switch_to_expired
FROM payment_position pp
WHERE po.switch_to_expired IS NULL and po.payment_position_id = pp.id;

--No, because v1.0.0 is continuing to doesnt write to po start
-- 2 for safety: any NULLs -> false
-- UPDATE payment_option
-- SET switch_to_expired = false
-- WHERE switch_to_expired IS NULL;
--No, because v1.0.0 is continuing to doesnt write to po end

-- 3 NOT NULL + default
ALTER TABLE payment_option
  ALTER COLUMN switch_to_expired SET NOT NULL,
  ALTER COLUMN switch_to_expired SET DEFAULT false;
  
-- 4 drop from payment_position
--ALTER TABLE payment_position
--  DROP COLUMN IF EXISTS switch_to_expired;