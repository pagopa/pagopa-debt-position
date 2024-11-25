DO $$
BEGIN
    -- Aggiornamento per 'WISP'
    UPDATE apd.payment_position
    SET service_type = 'WISP'
    WHERE iupd ILIKE 'wisp_%';

    -- Aggiornamento per 'ACA'
    UPDATE apd.payment_position
    SET service_type = 'ACA'
    WHERE iupd ILIKE 'aca_%';

EXCEPTION
    WHEN OTHERS THEN
        -- Gestione dell'errore
        RAISE NOTICE 'Errore durante l''esecuzione dello script. Tipo di errore: %, Messaggio di errore: %', SQLSTATE, SQLERRM;
        RAISE;
END;
$$;