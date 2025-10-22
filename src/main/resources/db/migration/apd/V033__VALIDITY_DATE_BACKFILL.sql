-- 1 backfill: Copy the validity_date from position to options only where it is missing
UPDATE payment_option po
SET validity_date = pp.validity_date
FROM payment_position pp
WHERE po.payment_position_id = pp.id
  AND po.validity_date IS NULL
  AND pp.validity_date IS NOT NULL;
  
-- 2 drop validity_date from payment_position
--ALTER TABLE payment_position
--  DROP COLUMN IF EXISTS validity_date;