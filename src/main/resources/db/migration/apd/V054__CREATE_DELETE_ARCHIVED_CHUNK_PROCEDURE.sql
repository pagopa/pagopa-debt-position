CREATE OR REPLACE PROCEDURE apd.delete_archived_chunk(p_limit INT, p_offset INT)
LANGUAGE plpgsql
AS $$
DECLARE
    v_pos_ids bigint[];
    v_opt_ids bigint[];
    v_trans_ids bigint[];
BEGIN
    -- 1. We load the chunk IDs into RAM
    SELECT array_agg(id) INTO v_pos_ids
    FROM (
        SELECT id FROM apd.archiving_selection_buffer
        ORDER BY id LIMIT p_limit OFFSET p_offset
    ) t;

    IF v_pos_ids IS NULL THEN RETURN; END IF;

    SELECT array_agg(id) INTO v_opt_ids
    FROM apd.payment_option WHERE payment_position_id = ANY(v_pos_ids);

    SELECT array_agg(id) INTO v_trans_ids
    FROM apd.transfer WHERE payment_option_id = ANY(v_opt_ids);

    -- 2. SIGNALING PHASE (UPDATE for the CDC)
    IF v_trans_ids IS NOT NULL THEN
        UPDATE apd.transfer SET archived = true WHERE id = ANY(v_trans_ids);
    END IF;
    IF v_opt_ids IS NOT NULL THEN
        UPDATE apd.payment_option SET archived = true WHERE id = ANY(v_opt_ids);
    END IF;
    UPDATE apd.payment_position SET archived = true WHERE id = ANY(v_pos_ids);

    -- 3. PURGE PHASE (DELETION)
    -- The order is reversed (bottom-up) to respect foreign keys.
    IF v_trans_ids IS NOT NULL THEN
        DELETE FROM apd.transfer_metadata WHERE transfer_id = ANY(v_trans_ids);
        DELETE FROM apd.transfer WHERE id = ANY(v_trans_ids);
    END IF;
    IF v_opt_ids IS NOT NULL THEN
        DELETE FROM apd.payment_option_metadata WHERE payment_option_id = ANY(v_opt_ids);
        DELETE FROM apd.payment_option WHERE id = ANY(v_opt_ids);
    END IF;
    DELETE FROM apd.payment_position WHERE id = ANY(v_pos_ids);

    COMMIT;
END;
$$;