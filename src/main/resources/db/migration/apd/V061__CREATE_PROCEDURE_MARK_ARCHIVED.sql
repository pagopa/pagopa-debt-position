CREATE OR REPLACE PROCEDURE apd.mark_archived_chunk(p_limit INT, p_offset INT)
LANGUAGE plpgsql
AS $$
DECLARE
    v_all_pos_ids bigint[];
    v_chunk_pos_ids bigint[];
    v_opt_ids bigint[];
    v_trans_ids bigint[];

    v_chunk_size INT := 100;   -- Processing of v_chunk_size=100 payment positions at time
    v_sleep_time FLOAT := 0.5; -- v_sleep_time=500ms pause to avoid peaks in resource consumption

    v_array_length INT;
    i INT;
BEGIN
    -- Load all root IDs
    SELECT array_agg(id) INTO v_all_pos_ids
    FROM (
        SELECT id FROM apd.archiving_selection_buffer
        ORDER BY id LIMIT p_limit OFFSET p_offset
    ) t;

    IF v_all_pos_ids IS NULL THEN
        RETURN;
    END IF;

    v_array_length := array_length(v_all_pos_ids, 1);

    -- micro-chunk cycle
    FOR i IN 1 .. v_array_length BY v_chunk_size LOOP

        -- Isolate the v_chunk_size current payment positions.
        v_chunk_pos_ids := v_all_pos_ids[i : i + v_chunk_size - 1];

        -- Compute v_opt_ids payment option ids.
        SELECT array_agg(id) INTO v_opt_ids
        FROM apd.payment_option
        WHERE payment_position_id = ANY(v_chunk_pos_ids);

        -- Compute v_trans_ids transfers ids.
        v_trans_ids := NULL;
        IF v_opt_ids IS NOT NULL THEN
            SELECT array_agg(id) INTO v_trans_ids
            FROM apd.transfer
            WHERE payment_option_id = ANY(v_opt_ids);
        END IF;

        -- ==========================================
        -- Update phase
        -- ==========================================

        -- We update starting from the transfers up to the payment positions.
        -- Same transaction (until COMMIT), so the application
        -- will never see the hierarchy in a mixed or partial state (consistency).
        IF v_trans_ids IS NOT NULL THEN
            UPDATE apd.transfer SET archived = true WHERE id = ANY(v_trans_ids);
        END IF;

        IF v_opt_ids IS NOT NULL THEN
            UPDATE apd.payment_option SET archived = true WHERE id = ANY(v_opt_ids);
        END IF;

        UPDATE apd.payment_position SET archived = true WHERE id = ANY(v_chunk_pos_ids);

        -- Commit micro-chunk
        COMMIT;

        -- Pausing to optimize CPU load for online queries.
        PERFORM pg_sleep(v_sleep_time);

    END LOOP;
END;
$$;
+2,
