CREATE TABLE IF NOT EXISTS apd.debezium_signal (
	id   VARCHAR(42)   NOT NULL PRIMARY KEY,
	type VARCHAR(32)   NOT NULL,
	data VARCHAR(2048)
);

DO $$
BEGIN
	IF EXISTS (
		SELECT 1
		FROM pg_publication p
		WHERE p.pubname = 'dbz_publication'
	)
	AND NOT EXISTS (
		SELECT 1
		FROM pg_publication_rel pr
		JOIN pg_publication p ON p.oid = pr.prpubid
		JOIN pg_class c ON c.oid = pr.prrelid
		JOIN pg_namespace n ON n.oid = c.relnamespace
		WHERE p.pubname = 'dbz_publication'
		  AND n.nspname = 'apd'
		  AND c.relname = 'debezium_signal'
	) THEN
		ALTER PUBLICATION dbz_publication ADD TABLE apd.debezium_signal;
	END IF;
END
$$;

GRANT SELECT, INSERT ON TABLE apd.debezium_signal TO cdcapd;
