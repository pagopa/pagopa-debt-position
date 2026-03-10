CREATE TABLE IF NOT EXISTS apd.archiving_staging (
    -- Auto-incremental ID
    id bigint GENERATED ALWAYS AS IDENTITY PRIMARY KEY,

    -- The ADF pipeline ID
    run_id text NOT NULL,

    -- The JSONB payload
    p_json_data jsonb NOT NULL,

	p_count INT
);