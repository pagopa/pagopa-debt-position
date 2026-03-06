-- CROSS JOIN LATERAL are used as 'for each elem'
-- https://www.postgresql.org/docs/current/queries-table-expressions.html#QUERIES-LATERAL
CREATE OR REPLACE PROCEDURE apd.ingest_payment_positions_bulk(p_json_data JSONB)
LANGUAGE plpgsql
AS $$
BEGIN
    -- Insert PAYMENT_POSITION
    -- Add ‘last_updated_date_pp’ to the JSON object before conversion
    INSERT INTO apd.payment_position
    SELECT pos.* FROM jsonb_array_elements(p_json_data) AS elem
    CROSS JOIN LATERAL jsonb_populate_record(
        NULL::apd.payment_position,
        (elem - 'options') || jsonb_build_object('last_updated_date_pp', elem->'last_updated_date')
    ) AS pos;

    -- Insert PAYMENT_OPTION
    -- Propagate last_updated_date from the root (elem) to the option
    INSERT INTO apd.payment_option
    SELECT opt.* FROM jsonb_array_elements(p_json_data) AS elem
    CROSS JOIN LATERAL jsonb_array_elements(elem->'options') AS opt_elem
    CROSS JOIN LATERAL jsonb_populate_record(
        NULL::apd.payment_option,
        (opt_elem - 'transfers' - 'metadata') || jsonb_build_object('last_updated_date_pp', elem->'last_updated_date')
    ) AS opt;

    -- Entering PAYMENT_OPTION_METADATA
    INSERT INTO apd.payment_option_metadata
    SELECT meta.* FROM jsonb_array_elements(p_json_data) AS elem
    CROSS JOIN LATERAL jsonb_array_elements(elem->'options') AS opt_elem
    CROSS JOIN LATERAL jsonb_array_elements(opt_elem->'metadata') AS meta_elem
    CROSS JOIN LATERAL jsonb_populate_record(
        NULL::apd.payment_option_metadata,
        meta_elem || jsonb_build_object('last_updated_date_pp', elem->'last_updated_date')
    ) AS meta
    WHERE opt_elem->'metadata' IS NOT NULL AND opt_elem->'metadata' != 'null'::jsonb;

    -- TRANSFER entry
    INSERT INTO apd.transfer
    SELECT tr.* FROM jsonb_array_elements(p_json_data) AS elem
    CROSS JOIN LATERAL jsonb_array_elements(elem->'options') AS opt_elem
    CROSS JOIN LATERAL jsonb_array_elements(opt_elem->'transfers') AS tr_elem
    CROSS JOIN LATERAL jsonb_populate_record(
        NULL::apd.transfer,
        (tr_elem - 'metadata') || jsonb_build_object('last_updated_date_pp', elem->'last_updated_date')
    ) AS tr
    WHERE opt_elem->'transfers' IS NOT NULL AND opt_elem->'transfers' != 'null'::jsonb;

    -- TRANSFER_METADATA entry
    INSERT INTO apd.transfer_metadata
    SELECT tm.* FROM jsonb_array_elements(p_json_data) AS elem
    CROSS JOIN LATERAL jsonb_array_elements(elem->'options') AS opt_elem
    CROSS JOIN LATERAL jsonb_array_elements(opt_elem->'transfers') AS tr_elem
    CROSS JOIN LATERAL jsonb_array_elements(tr_elem->'metadata') AS tm_elem
    CROSS JOIN LATERAL jsonb_populate_record(
        NULL::apd.transfer_metadata,
        tm_elem || jsonb_build_object('last_updated_date_pp', elem->'last_updated_date')
    ) AS tm
    WHERE tr_elem->'metadata' IS NOT NULL AND tr_elem->'metadata' != 'null'::jsonb;

END;
$$;