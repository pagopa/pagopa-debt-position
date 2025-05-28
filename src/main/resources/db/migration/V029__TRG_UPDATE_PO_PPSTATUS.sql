CREATE OR REPLACE FUNCTION apd.sync_status_from_position()
RETURNS TRIGGER AS $$
DECLARE
  pos_status TEXT;
BEGIN
  SELECT status INTO pos_status
  FROM apd.payment_position
  WHERE id = NEW.payment_position_id;
  IF pos_status IS NOT NULL THEN
    NEW.payment_position_status := pos_status;
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;
DROP TRIGGER IF EXISTS trg_sync_status_on_payment_option_insert ON apd.payment_option;
CREATE TRIGGER trg_sync_status_on_payment_option_insert
BEFORE INSERT ON apd.payment_option
FOR EACH ROW
WHEN (NEW.payment_position_id IS NOT NULL)
EXECUTE FUNCTION apd.sync_status_from_position();
CREATE OR REPLACE FUNCTION apd.update_options_on_position_status_change()
RETURNS TRIGGER AS $$
BEGIN
  IF NEW.status IS DISTINCT FROM OLD.status THEN
    UPDATE apd.payment_option
    SET payment_position_status = NEW.status,
        last_updated_date = now()
    WHERE payment_position_id = NEW.id;
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;
DROP TRIGGER IF EXISTS trg_update_options_on_position_status_change ON apd.payment_position;
CREATE TRIGGER trg_update_options_on_position_status_change
AFTER UPDATE OF status ON apd.payment_position
FOR EACH ROW
EXECUTE FUNCTION apd.update_options_on_position_status_change();