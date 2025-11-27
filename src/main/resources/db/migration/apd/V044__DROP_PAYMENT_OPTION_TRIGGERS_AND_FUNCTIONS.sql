-- 1. Drop triggers that use functions
DROP TRIGGER IF EXISTS trg_sync_status_on_payment_option_insert
    ON payment_option;
DROP TRIGGER IF EXISTS trg_update_options_on_position_status_change
    ON payment_position;

-- 2. Dropping functions used by triggers
DROP FUNCTION IF EXISTS sync_status_from_position();
DROP FUNCTION IF EXISTS update_options_on_position_status_change();