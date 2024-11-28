-- Primo step: aggiornamento per 'WISP'
DO $$
BEGIN
    -- Aggiornamento per 'WISP'
    UPDATE payment_position
    SET service_type = 'WISP'
    WHERE iupd ILIKE 'wisp_%';

EXCEPTION
    WHEN OTHERS THEN
        -- Gestione dell'errore
        RAISE NOTICE 'Errore durante l''aggiornamento per WISP. Tipo di errore: %, Messaggio di errore: %', SQLSTATE, SQLERRM;
        RAISE;
END;
$$;

-- Secondo step: aggiornamento per 'ACA'
DO $$
BEGIN
    -- Aggiornamento per 'ACA'
    UPDATE payment_position
    SET service_type = 'ACA'
    WHERE iupd ILIKE 'aca_%';

EXCEPTION
    WHEN OTHERS THEN
        -- Gestione dell'errore
        RAISE NOTICE 'Errore durante l''aggiornamento per ACA. Tipo di errore: %, Messaggio di errore: %', SQLSTATE, SQLERRM;
        RAISE;
END;
$$;