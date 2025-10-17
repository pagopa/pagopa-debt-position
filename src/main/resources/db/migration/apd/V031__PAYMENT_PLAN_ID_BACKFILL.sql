-- A) Single options: always NULL
UPDATE apd.installment
SET payment_plan_id = NULL
WHERE is_partial_payment IS NOT TRUE;

-- B) Installments: one UUID for *payment_position* (only when payment_plan_id is NULL)
WITH one_uuid_per_position AS (
  SELECT
    payment_position_id,
    gen_random_uuid()::text AS u
  FROM apd.installment
  WHERE is_partial_payment IS TRUE
  GROUP BY payment_position_id
)
UPDATE apd.installment i
SET payment_plan_id = p.u
FROM one_uuid_per_position p
WHERE i.is_partial_payment IS TRUE
  AND i.payment_position_id = p.payment_position_id
  AND i.payment_plan_id IS NULL;