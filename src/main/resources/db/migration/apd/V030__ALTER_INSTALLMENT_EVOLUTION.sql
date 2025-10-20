-- =========================
-- PO → INSTALLMENT RENAME
-- =========================

-- ALTER TABLE IF EXISTS payment_option RENAME TO installment;
-- ALTER TABLE IF EXISTS installment RENAME CONSTRAINT payment_option_pkey TO installment_pkey;
-- ALTER TABLE IF EXISTS installment RENAME CONSTRAINT uniquepaymentopt TO uniqueinstallmentiuv;
-- ALTER TABLE IF EXISTS installment RENAME CONSTRAINT uniquepaymentoptnav TO uniqueinstallmentnav;

-- ALTER INDEX IF EXISTS idx_payment_option_inserted_date RENAME TO idx_installment_inserted_date;

DROP INDEX IF EXISTS payment_option_payment_position_id_idx;

-- ALTER SEQUENCE IF EXISTS payment_opt_seq RENAME TO installment_seq;

ALTER TABLE payment_option ADD COLUMN IF NOT EXISTS payment_plan_id TEXT;

ALTER TABLE payment_option ADD COLUMN IF NOT EXISTS switch_to_expired boolean;

-- (Optional but useful) path : get all installments of a position/plan sorted by due_date
-- CREATE INDEX IF NOT EXISTS idx_inst_pos_plan_due
  --ON apd.installment (payment_position_id, payment_plan_id, due_date);

-- =========================
-- PO METADATA → INSTALLMENT METADATA
-- =========================

-- ALTER TABLE IF EXISTS payment_option_metadata RENAME TO installment_metadata;
-- ALTER TABLE IF EXISTS installment_metadata RENAME COLUMN payment_option_id TO installment_id;
-- ALTER TABLE IF EXISTS installment_metadata RENAME CONSTRAINT payment_option_metadata_pkey TO installment_metadata_pkey;
-- ALTER TABLE IF EXISTS installment_metadata RENAME CONSTRAINT uniquepaymentoptmetadata TO uniqueinstallmentmetadata;

-- DROP INDEX IF EXISTS idx_payment_option_metadata_payment_option_id;
-- CREATE INDEX IF NOT EXISTS idx_installment_metadata_installment_id ON installment_metadata (installment_id);

-- ALTER SEQUENCE IF EXISTS payment_opt_metadata_seq RENAME TO installment_metadata_seq;

-- =========================
-- send_sync PROCEDURE UPDATE
-- =========================
-- Update the procedure for apd.installment
-- DROP PROCEDURE IF EXISTS apd.update_payment_option_send_sync(INTEGER);

/*CREATE OR REPLACE PROCEDURE apd.update_payment_option_send_sync(IN batch_size integer)
LANGUAGE plpgsql
AS $procedure$
DECLARE
  updated_rows INTEGER := 0;
BEGIN
  LOOP
    WITH to_update AS (
      SELECT id
      FROM apd.installment
      WHERE send_sync IS NULL
      LIMIT batch_size
    )
    UPDATE apd.installment
       SET send_sync = false
     WHERE id IN (SELECT id FROM to_update);

    GET DIAGNOSTICS updated_rows = ROW_COUNT;
    EXIT WHEN updated_rows = 0;

    PERFORM pg_sleep(0.1); -- short pause between batches
  END LOOP;
END;
$procedure$;*/

-- =========================
-- update_options_on_position_status_change TRIGGER
-- =========================
/*CREATE OR REPLACE FUNCTION apd.update_options_on_position_status_change()
RETURNS trigger
LANGUAGE plpgsql
AS $function$
BEGIN
  IF NEW.status IS DISTINCT FROM OLD.status THEN
    UPDATE apd.installment
    SET payment_position_status = NEW.status,
        last_updated_date = now()
    WHERE payment_position_id = NEW.id;
  END IF;
  RETURN NEW;
END;
$function$;*/