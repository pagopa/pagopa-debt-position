CREATE OR REPLACE PROCEDURE apd.rollback_ingest_payment_positions(
    p_migration_run_id TEXT,
    p_last_updated_from TIMESTAMP DEFAULT NULL,
    p_last_updated_to TIMESTAMP DEFAULT NULL
)
LANGUAGE plpgsql
AS $$
BEGIN
    -- Security check
    IF p_migration_run_id IS NULL OR TRIM(p_migration_run_id) = '' THEN
        RAISE EXCEPTION 'p_migration_run_id cannot be null or empty';
    END IF;

    -- If the parameters are NULL, apply the default: from 10 years ago to 2 years ago.
    p_last_updated_from := COALESCE(p_last_updated_from, CURRENT_TIMESTAMP - INTERVAL '10 years');
    p_last_updated_to   := COALESCE(p_last_updated_to, CURRENT_TIMESTAMP - INTERVAL '2 years');

    -- Perform DELETE operations using partition pruning based on the value of p_last_updated
    DELETE FROM apd.transfer_metadata
    WHERE migration_run_id = p_migration_run_id
      AND last_updated_date_pp >= p_last_updated_from
      AND last_updated_date_pp <= p_last_updated_to;

    DELETE FROM apd.transfer
    WHERE migration_run_id = p_migration_run_id
      AND last_updated_date_pp >= p_last_updated_from
      AND last_updated_date_pp <= p_last_updated_to;

    DELETE FROM apd.payment_option_metadata
    WHERE migration_run_id = p_migration_run_id
      AND last_updated_date_pp >= p_last_updated_from
      AND last_updated_date_pp <= p_last_updated_to;

    DELETE FROM apd.payment_option
    WHERE migration_run_id = p_migration_run_id
      AND last_updated_date_pp >= p_last_updated_from
      AND last_updated_date_pp <= p_last_updated_to;

    DELETE FROM apd.payment_position
    WHERE migration_run_id = p_migration_run_id
      AND last_updated_date_pp >= p_last_updated_from
      AND last_updated_date_pp <= p_last_updated_to;

END;
$$;