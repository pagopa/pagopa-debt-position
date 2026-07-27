CREATE OR REPLACE PROCEDURE apd.delete_archived_chunk(p_pos_ids bigint[])
LANGUAGE plpgsql
AS $$
DECLARE
    v_opt_ids bigint[];
    v_trans_ids bigint[];
BEGIN
    -- Security check: exit if the input array is null or empty
    IF p_pos_ids IS NULL OR array_length(p_pos_ids, 1) IS NULL THEN
        RETURN;
    END IF;

    -- Retrieve the IDs of the children (Options and Transfers) based on the PaymentPositions
    SELECT array_agg(id) INTO v_opt_ids
    FROM apd.payment_option WHERE payment_position_id = ANY(p_pos_ids);

    IF v_opt_ids IS NOT NULL THEN
        SELECT array_agg(id) INTO v_trans_ids
        FROM apd.transfer WHERE payment_option_id = ANY(v_opt_ids);
    END IF;

    -- 2. SIGNALING PHASE (UPDATE for the CDC)
    IF v_trans_ids IS NOT NULL THEN
        UPDATE apd.transfer SET archived = true WHERE id = ANY(v_trans_ids);
    END IF;

    IF v_opt_ids IS NOT NULL THEN
        UPDATE apd.payment_option SET archived = true WHERE id = ANY(v_opt_ids);
    END IF;

    UPDATE apd.payment_position SET archived = true WHERE id = ANY(p_pos_ids);

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

    DELETE FROM apd.payment_position WHERE id = ANY(p_pos_ids);

    COMMIT;
END;
$$;