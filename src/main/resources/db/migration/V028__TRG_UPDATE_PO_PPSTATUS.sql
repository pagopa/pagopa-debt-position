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
-- Trigger that calls the function after every update on the payment_position table
CREATE TRIGGER trg_sync_status_on_payment_option
BEFORE INSERT OR UPDATE ON payment_option
FOR EACH ROW
WHEN (NEW.payment_position_id IS NOT NULL)
EXECUTE FUNCTION sync_status_from_position();