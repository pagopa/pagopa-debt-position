CREATE OR REPLACE FUNCTION apd.get_archiving_chunk(p_limit INT, p_offset INT)
RETURNS TABLE(p_json_data JSONB)
LANGUAGE plpgsql AS $$
BEGIN
    RETURN QUERY
    SELECT jsonb_agg(root)
    FROM (
        SELECT pos.*,
            (SELECT jsonb_agg(opt_bundle)
             FROM (
                 SELECT opt.*,
                     (SELECT jsonb_agg(om.*) FROM apd.payment_option_metadata om WHERE om.payment_option_id = opt.id) as metadata,
                     (SELECT jsonb_agg(tr_bundle)
                      FROM (
                          SELECT tr.*,
                              (SELECT jsonb_agg(tm.*) FROM apd.transfer_metadata tm WHERE tm.transfer_id = tr.id) as metadata
                          FROM apd.transfer tr WHERE tr.payment_option_id = opt.id
                      ) tr_bundle
                     ) as transfers
                 FROM apd.payment_option opt WHERE opt.payment_position_id = pos.id
             ) opt_bundle
            ) as options
        FROM apd.payment_position pos
        WHERE pos.id IN (
            -- Navigate through the view materialized as a sequential list.
            SELECT id
            FROM apd.archiving_selection_buffer
            ORDER BY id
            LIMIT p_limit
            OFFSET p_offset
        )
    ) root;
END; $$;