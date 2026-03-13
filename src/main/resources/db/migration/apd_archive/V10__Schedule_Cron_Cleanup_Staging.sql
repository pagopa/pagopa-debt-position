SELECT cron.schedule(
    'cleanup_staging_job',
    '45 * * * *',                       -- At 45 minutes past the hour.
    'CALL apd.cleanup_staging_table(24);' -- The SQL command to be executed, cleaning records dating back 24 hours.
);