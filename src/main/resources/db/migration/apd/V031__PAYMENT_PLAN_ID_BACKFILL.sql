CREATE EXTENSION IF NOT EXISTS pgcrypto;

-- A) Single options: UUID per line
UPDATE apd.installment i
SET payment_plan_id = '0:' || gen_random_uuid()::text
WHERE i.is_partial_payment IS NOT TRUE
  AND i.payment_plan_id IS NULL;
  
 -- B) Installments: one UUID for *payment_position*
WITH one_uuid_per_position AS (
  SELECT DISTINCT payment_position_id,
         gen_random_uuid()::text AS u
  FROM apd.installment
  WHERE is_partial_payment IS TRUE
)
UPDATE apd.installment i
SET payment_plan_id = '1:' || p.u
FROM one_uuid_per_position p
WHERE i.is_partial_payment IS TRUE
  AND i.payment_position_id = p.payment_position_id
  AND i.payment_plan_id IS NULL;

ALTER TABLE apd.installment ALTER COLUMN payment_plan_id SET NOT NULL;