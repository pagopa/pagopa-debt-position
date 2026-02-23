CREATE OR REPLACE PROCEDURE apd.delete_archived_chunk(p_limit INT, p_offset INT)
LANGUAGE plpgsql
AS $$
BEGIN
    WITH targeted_ids AS (
        SELECT id
        FROM apd.archiving_selection_buffer
        ORDER BY id
        LIMIT p_limit
        OFFSET p_offset
    ),
    targeted_options AS (
        SELECT id FROM apd.payment_option
        WHERE payment_position_id IN (SELECT id FROM targeted_ids)
    ),
    targeted_transfers AS (
        SELECT id FROM apd.transfer
        WHERE payment_option_id IN (SELECT id FROM targeted_options)
    ),
    del_transfer_metadata AS (
        DELETE FROM apd.transfer_metadata
        WHERE transfer_id IN (SELECT id FROM targeted_transfers)
    ),
    del_transfers AS (
        DELETE FROM apd.transfer
        WHERE id IN (SELECT id FROM targeted_transfers)
    ),
    del_option_metadata AS (
        DELETE FROM apd.payment_option_metadata
        WHERE payment_option_id IN (SELECT id FROM targeted_options)
    ),
    del_options AS (
        DELETE FROM apd.payment_option
        WHERE id IN (SELECT id FROM targeted_options)
    )
    DELETE FROM apd.payment_position
    WHERE id IN (SELECT id FROM targeted_ids);

    COMMIT;
END;
$$;