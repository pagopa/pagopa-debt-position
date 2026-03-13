CREATE OR REPLACE PROCEDURE apd.delete_archived_chunk(p_limit INT, p_offset INT)
LANGUAGE plpgsql
AS $$
DECLARE
    v_batch_size INT := 1000;
    v_processed INT := 0;
    v_current_limit INT;
    v_pos_ids bigint[];
BEGIN
    -- The cycle runs until we have exhausted the total block required (e.g., 10,000).
    WHILE v_processed < p_limit LOOP

        -- Compute the size of the micro-chunk (maximum 1000, or less if we are at the end).
        v_current_limit := LEAST(v_batch_size, p_limit - v_processed);

        -- Extraction of the micro-chunk from the Materialized View
        -- The offset advances based on how many records we have already processed in this execution
        SELECT array_agg(id) INTO v_pos_ids
        FROM (
            SELECT id FROM apd.archiving_selection_buffer
            ORDER BY id
            LIMIT v_current_limit
            OFFSET p_offset + v_processed
        ) t;

        -- If the view is empty or the records have ended, the loop is interrupted.
        IF v_pos_ids IS NULL OR array_length(v_pos_ids, 1) IS NULL THEN
            EXIT;
        END IF;

        -- DELETION PHASE
        DELETE FROM apd.transfer_metadata tm
        USING apd.transfer t, apd.payment_option po, unnest(v_pos_ids) AS p(id)
        WHERE tm.transfer_id = t.id
          AND t.payment_option_id = po.id
          AND po.payment_position_id = p.id;

        DELETE FROM apd.transfer t
        USING apd.payment_option po, unnest(v_pos_ids) AS p(id)
        WHERE t.payment_option_id = po.id
          AND po.payment_position_id = p.id;

        DELETE FROM apd.payment_option_metadata pom
        USING apd.payment_option po, unnest(v_pos_ids) AS p(id)
        WHERE pom.payment_option_id = po.id
          AND po.payment_position_id = p.id;

        DELETE FROM apd.payment_option po
        USING unnest(v_pos_ids) AS p(id)
        WHERE po.payment_position_id = p.id;

        DELETE FROM apd.payment_position pp
        USING unnest(v_pos_ids) AS p(id)
        WHERE pp.id = p.id;

        -- Commit micro-chunk
        COMMIT;

        -- Counter updated and progress print
        v_processed := v_processed + v_current_limit;
		RAISE NOTICE 'Micro-batch completed: % of % total records deleted and committed.', v_processed, p_limit;

    END LOOP;
END;
$$;