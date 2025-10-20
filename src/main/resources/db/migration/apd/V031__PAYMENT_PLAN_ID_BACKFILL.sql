-- A) Single options: always NULL
UPDATE payment_option
SET payment_plan_id = NULL
WHERE is_partial_payment IS NOT TRUE;

-- B) payment options: one UUID for *payment_position* (only when payment_plan_id is NULL)
WITH one_uuid_per_position AS (
  SELECT
    payment_position_id,
    gen_random_uuid()::text AS u
  FROM payment_option
  WHERE is_partial_payment IS TRUE
  GROUP BY payment_position_id
)
UPDATE payment_option po
SET payment_plan_id = p.u
FROM one_uuid_per_position p
WHERE po.is_partial_payment IS TRUE
  AND po.payment_position_id = p.payment_position_id
  AND po.payment_plan_id IS NULL;