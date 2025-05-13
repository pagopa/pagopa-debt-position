-- Function that performs update on payment_option
CREATE OR REPLACE FUNCTION sync_status_from_position()
RETURNS TRIGGER AS $$
DECLARE
  pos_status TEXT;
BEGIN
  SELECT status INTO pos_status
  FROM payment_position
  WHERE id = NEW.payment_position_id;

  IF pos_status IS NOT NULL THEN
    NEW.payment_position_status := pos_status;
  END IF;

  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION update_options_on_position_status_change()
RETURNS TRIGGER AS $$
BEGIN
  IF NEW.status IS DISTINCT FROM OLD.status THEN
    UPDATE payment_option
    SET payment_position_status = NEW.status,
        last_updated_date = now()
    WHERE payment_position_id = NEW.id;
  END IF;

  RETURN NEW;
END;
$$ LANGUAGE plpgsql;


-- Trigger that calls the function after every update on the payment_position table
DROP TRIGGER IF EXISTS trg_sync_status_on_payment_option_insert ON payment_option;

CREATE TRIGGER trg_sync_status_on_payment_option_insert
BEFORE INSERT ON payment_option
FOR EACH ROW
WHEN (NEW.payment_position_id IS NOT NULL)
EXECUTE FUNCTION sync_status_from_position();

DROP TRIGGER IF EXISTS trg_update_options_on_position_status_change ON payment_position;

CREATE TRIGGER trg_update_options_on_position_status_change
AFTER UPDATE OF status ON payment_position
FOR EACH ROW
EXECUTE FUNCTION update_options_on_position_status_change();