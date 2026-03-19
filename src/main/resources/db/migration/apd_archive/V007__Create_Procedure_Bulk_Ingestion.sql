-- CROSS JOIN LATERAL are used as 'for each elem'
-- https://www.postgresql.org/docs/current/queries-table-expressions.html#QUERIES-LATERAL
CREATE OR REPLACE PROCEDURE apd.ingest_payment_positions_bulk_n_rows(
    p_migration_run_id TEXT
)
LANGUAGE plpgsql
AS $$
BEGIN
    -- Security check to avoid empty runs
    IF p_migration_run_id IS NULL OR TRIM(p_migration_run_id) = '' THEN
        RAISE EXCEPTION 'p_migration_run_id cannot be null or empty';
    END IF;

    -- ==========================================
    -- 1. Insert PAYMENT_POSITION
    -- ==========================================
    INSERT INTO apd.payment_position
    SELECT pos.* FROM apd.archiving_staging stg
    CROSS JOIN LATERAL jsonb_populate_record(
        NULL::apd.payment_position,
        (stg.p_json_data - 'options') || jsonb_build_object(
            'last_updated_date_pp', stg.p_json_data->'last_updated_date',
            'migration_run_id', p_migration_run_id,
            'archiving_timestamp', CURRENT_TIMESTAMP
        )
    ) AS pos
    WHERE stg.run_id = p_migration_run_id;

    -- ==========================================
    -- 2. Insert PAYMENT_OPTION
    -- ==========================================
    INSERT INTO apd.payment_option
    SELECT opt.* FROM apd.archiving_staging stg
    CROSS JOIN LATERAL jsonb_array_elements(stg.p_json_data->'options') AS opt_elem
    CROSS JOIN LATERAL jsonb_populate_record(
        NULL::apd.payment_option,
        (opt_elem - 'transfers' - 'metadata') || jsonb_build_object(
            'last_updated_date_pp', stg.p_json_data->'last_updated_date',
            'migration_run_id', p_migration_run_id
        )
    ) AS opt
    WHERE stg.run_id = p_migration_run_id;

    -- ==========================================
    -- 3. Insert PAYMENT_OPTION_METADATA
    -- ==========================================
    INSERT INTO apd.payment_option_metadata
    SELECT meta.* FROM apd.archiving_staging stg
    CROSS JOIN LATERAL jsonb_array_elements(stg.p_json_data->'options') AS opt_elem
    CROSS JOIN LATERAL jsonb_array_elements(opt_elem->'metadata') AS meta_elem
    CROSS JOIN LATERAL jsonb_populate_record(
        NULL::apd.payment_option_metadata,
        meta_elem || jsonb_build_object(
            'last_updated_date_pp', stg.p_json_data->'last_updated_date',
            'migration_run_id', p_migration_run_id
        )
    ) AS meta
    WHERE stg.run_id = p_migration_run_id
      AND opt_elem->'metadata' IS NOT NULL AND opt_elem->'metadata' != 'null'::jsonb;

    -- ==========================================
    -- 4. Insert TRANSFER
    -- ==========================================
    INSERT INTO apd.transfer
    SELECT tr.* FROM apd.archiving_staging stg
    CROSS JOIN LATERAL jsonb_array_elements(stg.p_json_data->'options') AS opt_elem
    CROSS JOIN LATERAL jsonb_array_elements(opt_elem->'transfers') AS tr_elem
    CROSS JOIN LATERAL jsonb_populate_record(
        NULL::apd.transfer,
        (tr_elem - 'metadata') || jsonb_build_object(
            'last_updated_date_pp', stg.p_json_data->'last_updated_date',
            'migration_run_id', p_migration_run_id
        )
    ) AS tr
    WHERE stg.run_id = p_migration_run_id
      AND opt_elem->'transfers' IS NOT NULL AND opt_elem->'transfers' != 'null'::jsonb;

    -- ==========================================
    -- 5. Insert TRANSFER_METADATA
    -- ==========================================
    INSERT INTO apd.transfer_metadata
    SELECT tm.* FROM apd.archiving_staging stg
    CROSS JOIN LATERAL jsonb_array_elements(stg.p_json_data->'options') AS opt_elem
    CROSS JOIN LATERAL jsonb_array_elements(opt_elem->'transfers') AS tr_elem
    CROSS JOIN LATERAL jsonb_array_elements(tr_elem->'metadata') AS tm_elem
    CROSS JOIN LATERAL jsonb_populate_record(
        NULL::apd.transfer_metadata,
        tm_elem || jsonb_build_object(
            'last_updated_date_pp', stg.p_json_data->'last_updated_date',
            'migration_run_id', p_migration_run_id
        )
    ) AS tm
    WHERE stg.run_id = p_migration_run_id
      AND tr_elem->'metadata' IS NOT NULL AND tr_elem->'metadata' != 'null'::jsonb;

    -- ==========================================
    -- 6. Clean Staging Area
    -- ==========================================
    DELETE FROM apd.archiving_staging WHERE run_id = p_migration_run_id;

END;
$$;