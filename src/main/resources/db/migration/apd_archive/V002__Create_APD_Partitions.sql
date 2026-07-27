DO $$
DECLARE
    v_table_name text;
    -- List of tables to be partitioned
    v_tables text[] := ARRAY[
        'apd.payment_position',
        'apd.payment_option',
        'apd.payment_option_metadata',
        'apd.transfer',
        'apd.transfer_metadata'
    ];
BEGIN
    FOREACH v_table_name IN ARRAY v_tables
    LOOP
        -- Check if the table is already managed by pg_partman
        IF NOT EXISTS (
            SELECT 1
            FROM partman.part_config
            WHERE parent_table = v_table_name
        ) THEN
            -- If it does not exist, we proceed with its creation.
            -- pg_partman configuration on the _pp column (Interval: 1 month)
            PERFORM partman.create_parent(
                p_parent_table := v_table_name,
                p_control := 'last_updated_date_pp',
                p_interval := '1 month',
                p_start_partition := '2016-01-01 00:00:00', -- Start from p_start_partition (10 years of retention)
                p_premake := 4                              -- Create 4 months partitions in the future
            );

            RAISE NOTICE 'pg_partman configuration created for: %', v_table_name;
        ELSE
            -- If it already exists, we report it in the logs and move on to the next one.
            RAISE NOTICE 'The % table is already configured in pg_partman. Skipped.', v_table_name;
        END IF;
    END LOOP;
END
$$;