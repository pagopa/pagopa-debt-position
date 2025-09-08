-- =====================================================
-- Schema: odp
-- Debt Position - Safe Script
-- =====================================================

-- =====================
-- sequences
-- =====================

CREATE SEQUENCE IF NOT EXISTS odp.payment_pos_seq
	INCREMENT BY 1
	MINVALUE 1
	MAXVALUE 9223372036854775807
	START 1
	CACHE 1
	NO CYCLE;

CREATE SEQUENCE IF NOT EXISTS odp.payment_opt_seq
	INCREMENT BY 1
	MINVALUE 1
	MAXVALUE 9223372036854775807
	START 1
	CACHE 1
	NO CYCLE;
	
CREATE SEQUENCE IF NOT EXISTS odp.installment_seq
	INCREMENT BY 1
	MINVALUE 1
	MAXVALUE 9223372036854775807
	START 1
	CACHE 1
	NO CYCLE;
	
CREATE SEQUENCE IF NOT EXISTS odp.transfer_seq
	INCREMENT BY 1
	MINVALUE 1
	MAXVALUE 9223372036854775807
	START 1
	CACHE 1
	NO CYCLE;
	
CREATE SEQUENCE IF NOT EXISTS odp.payment_opt_metadata_seq
	INCREMENT BY 1
	MINVALUE 1
	MAXVALUE 9223372036854775807
	START 1
	CACHE 1
	NO CYCLE;
	
CREATE SEQUENCE IF NOT EXISTS odp.transfer_metadata_seq
	INCREMENT BY 1
	MINVALUE 1
	MAXVALUE 9223372036854775807
	START 1
	CACHE 1
	NO CYCLE;
	
-- =====================
-- payment_position
-- =====================
CREATE TABLE IF NOT EXISTS odp.payment_position (
    id int8 NOT NULL,
    iupd varchar(100) NOT NULL,
    organization_fiscal_code varchar(50) NOT NULL,
    company_name varchar(100) NOT NULL,
    office_name varchar(255) NULL,
    inserted_date timestamp NOT NULL,
    payment_date timestamp NULL,
    last_updated_date timestamp NOT NULL,
    max_due_date timestamp NOT NULL,
    min_due_date timestamp NOT NULL,
    publish_date timestamp NULL,
    validity_date timestamp NULL,
    status varchar(25) NOT NULL,
    "version" int4 DEFAULT 0 NOT NULL,
    pull bool DEFAULT true NOT NULL,
    pay_stand_in bool DEFAULT true NOT NULL,
    service_type varchar(100) DEFAULT 'GPD'::character varying NOT NULL,
    CONSTRAINT payment_position_pkey PRIMARY KEY (id),
    CONSTRAINT uniquepaymentpos UNIQUE (iupd, organization_fiscal_code)
);

-- Index
CREATE INDEX IF NOT EXISTS idx_iupd ON odp.payment_position (iupd);
CREATE INDEX IF NOT EXISTS idx_organization_fiscal_code ON odp.payment_position (organization_fiscal_code);
CREATE INDEX IF NOT EXISTS idx_company_name ON odp.payment_position (company_name);
CREATE INDEX IF NOT EXISTS idx_payment_date ON odp.payment_position (payment_date);
CREATE INDEX IF NOT EXISTS idx_status_validity_date ON odp.payment_position (status, validity_date)

-- Function + Trigger
CREATE OR REPLACE FUNCTION odp.update_options_on_position_status_change()
 RETURNS trigger
 LANGUAGE plpgsql
AS $function$
BEGIN
  IF NEW.status IS DISTINCT FROM OLD.status THEN
    UPDATE odp.payment_option
    SET payment_position_status = NEW.status,
        last_updated_date = now()
    WHERE payment_position_id = NEW.id;
  END IF;
  RETURN NEW;
END;
$function$;

DO $$
BEGIN
    IF NOT EXISTS (
        SELECT 1 FROM pg_trigger 
        WHERE tgname = 'trg_update_options_on_position_status_change'
    ) THEN
        CREATE TRIGGER trg_update_options_on_position_status_change
        AFTER UPDATE OF status ON odp.payment_position
        FOR EACH ROW EXECUTE FUNCTION odp.update_options_on_position_status_change();
    END IF;
END$$;

-- =====================
-- payment_option
-- =====================
CREATE TABLE IF NOT EXISTS odp.payment_option (
    id int8 NOT NULL,
    organization_fiscal_code varchar(50) NOT NULL,
    payment_position_id int8 NOT NULL,
    retention_date timestamp NULL,
    validity_date timestamp NULL,
    description varchar(255) NOT NULL,
    inserted_date timestamp NOT NULL,
    switch_to_expired bool DEFAULT false NOT NULL,
    -- Denormalized debtor data
    debtor_fiscal_code varchar(255) NOT NULL,
    debtor_type varchar(255) NOT NULL,
    debtor_full_name varchar(255) NOT NULL,
    debtor_street_name varchar(255) NULL,
    debtor_civic_number varchar(255) NULL,
    debtor_postal_code varchar(255) NULL,
    debtor_city varchar(255) NULL,
    debtor_province varchar(255) NULL,
    debtor_region varchar(255) NULL,
    debtor_country varchar(255) NULL,
    debtor_email varchar(255) NULL,
    debtor_phone varchar(255) NULL,
    CONSTRAINT payment_option_pkey PRIMARY KEY (id),
    CONSTRAINT uniquepaymentopt UNIQUE (organization_fiscal_code, payment_position_id),
    CONSTRAINT fk_payment_position_id FOREIGN KEY (payment_position_id) REFERENCES odp.payment_position(id)
);

-- Index
CREATE INDEX IF NOT EXISTS idx_payment_position_id ON odp.payment_option (payment_position_id);
CREATE INDEX IF NOT EXISTS idx_debtor_fiscal_code ON odp.payment_option (debtor_fiscal_code);

-- Function + Trigger
CREATE OR REPLACE FUNCTION odp.sync_status_from_position()
 RETURNS trigger
 LANGUAGE plpgsql
AS $function$
DECLARE
  pos_status TEXT;
BEGIN
  SELECT status INTO pos_status
  FROM odp.payment_position
  WHERE id = NEW.payment_position_id;
  IF pos_status IS NOT NULL THEN
    NEW.payment_position_status := pos_status;
  END IF;
  RETURN NEW;
END;
$function$;

DO $$
BEGIN
    IF NOT EXISTS (
        SELECT 1 FROM pg_trigger 
        WHERE tgname = 'trg_sync_status_on_payment_option_insert'
    ) THEN
        CREATE TRIGGER trg_sync_status_on_payment_option_insert
        BEFORE INSERT ON odp.payment_option
        FOR EACH ROW WHEN ((NEW.payment_position_id IS NOT NULL))
        EXECUTE FUNCTION odp.sync_status_from_position();
    END IF;
END$$;

-- =====================
-- installment
-- =====================
CREATE TABLE IF NOT EXISTS odp.installment (
    id int8 NOT NULL,
    nav varchar(25) NOT NULL,
    iuv varchar(25) NOT NULL,
    organization_fiscal_code varchar(50) NOT NULL,
    payment_option_id int8 NOT NULL,
    payment_position_id int8 NOT NULL,
    due_date timestamp NOT NULL,
    amount int8 NOT NULL,
    description varchar(255) NULL,
    fee int8 NULL,
    flow_reporting_id varchar(255) NULL,
    receipt_id varchar(255) NULL,
    inserted_date timestamp NOT NULL,
    last_updated_date timestamp NOT NULL,
    last_updated_date_notification_fee timestamp NULL,
    payment_date timestamp NULL,
    payment_method varchar(255) NULL,
    psp_company varchar(255) NULL,
    reporting_date timestamp NULL,
    status varchar(25) NULL,
    notification_fee int8 DEFAULT 0 NOT NULL,
    CONSTRAINT installment_pkey PRIMARY KEY (id),
    CONSTRAINT uniqueinstallmentnav UNIQUE (nav, organization_fiscal_code),
    CONSTRAINT uniqueinstallmentiuv UNIQUE (iuv, organization_fiscal_code),
    CONSTRAINT fk_payment_option_id FOREIGN KEY (payment_option_id) REFERENCES odp.payment_option(id)
);

-- Index
CREATE INDEX IF NOT EXISTS idx_due_date ON odp.installment (due_date);
CREATE INDEX IF NOT EXISTS idx_payment_option_id_inst ON odp.installment (payment_option_id);
CREATE INDEX IF NOT EXISTS idx_payment_position_id_inst ON odp.installment (payment_position_id);

-- =====================
-- transfer
-- =====================
CREATE TABLE IF NOT EXISTS odp.transfer (
    id int8 NOT NULL,
    transfer_id varchar(255) NOT NULL,
    iuv varchar(25) NOT NULL,
    installment_id int8 NOT NULL,
    amount int8 NOT NULL,
    category varchar(255) NOT NULL,
    iban varchar(255) NULL,
    inserted_date timestamp NOT NULL,
    last_updated_date timestamp NOT NULL,
    organization_fiscal_code varchar(50) NOT NULL,
    postal_iban varchar(255) NULL,
    remittance_information varchar(255) NOT NULL,
    status varchar(25) NOT NULL,
    hash_document varchar(255) NULL,
    stamp_type varchar(255) NULL,
    provincial_residence varchar(255) NULL,
    CONSTRAINT transfer_pkey PRIMARY KEY (id),
    CONSTRAINT uniquetransfer UNIQUE (iuv, organization_fiscal_code, transfer_id, installment_id),
    CONSTRAINT fk_installment_id FOREIGN KEY (installment_id) REFERENCES odp.installment(id)
);

-- Index
CREATE INDEX IF NOT EXISTS idx_installment_id ON odp.transfer (installment_id);

-- =====================
-- payment_option_metadata
-- =====================
CREATE TABLE IF NOT EXISTS odp.payment_option_metadata (
    id int8 NOT NULL,
    "key" varchar(140) NOT NULL,
    value varchar(140) NULL,
    payment_option_id int8 NOT NULL,
    CONSTRAINT payment_option_metadata_pkey PRIMARY KEY (id),
    CONSTRAINT uniquepaymentoptmetadata UNIQUE (key, payment_option_id),
    CONSTRAINT fk_payment_option_id FOREIGN KEY (payment_option_id) REFERENCES odp.payment_option(id)
);

-- Index
CREATE INDEX IF NOT EXISTS idx_payment_option_metadata_payment_option_id ON odp.payment_option_metadata (payment_option_id);


-- =====================
-- transfer_metadata
-- =====================
CREATE TABLE IF NOT EXISTS odp.transfer_metadata (
    id int8 NOT NULL,
    "key" varchar(140) NOT NULL,
    value varchar(140) NULL,
    transfer_id int8 NOT NULL,
    CONSTRAINT transfer_metadata_pkey PRIMARY KEY (id),
    CONSTRAINT uniquetransfermetadata UNIQUE (key, transfer_id),
    CONSTRAINT fk_transfer_id FOREIGN KEY (transfer_id) REFERENCES odp.transfer(id)
);

-- Index
CREATE INDEX IF NOT EXISTS idx_transfer_id ON odp.transfer_metadata (transfer_id);


-- =====================
-- shedlock
-- =====================

-- see: https://www.springcloud.io/post/2022-07/shedlock/#gsc.tab=0 
CREATE TABLE IF NOT EXISTS odp.shedlock (
  name VARCHAR(64),
  lock_until TIMESTAMP(3) NULL,
  locked_at TIMESTAMP(3) NULL,
  locked_by VARCHAR(255),
  PRIMARY KEY (name)
);
	
