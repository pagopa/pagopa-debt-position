-- =============================================================================
-- EXTENSIONS AND SCHEMA SETUP
-- =============================================================================
CREATE SCHEMA IF NOT EXISTS partman;
CREATE EXTENSION IF NOT EXISTS pg_partman SCHEMA partman;
CREATE EXTENSION IF NOT EXISTS pg_cron;

CREATE SCHEMA IF NOT EXISTS apd;

-- =============================================================================
-- TABLE: PAYMENT_POSITION
-- =============================================================================
CREATE TABLE apd.payment_position (
    id bigint NOT NULL,
    organization_fiscal_code character varying(255) NOT NULL,
    last_updated_date timestamp without time zone NOT NULL,
    iupd character varying(255) NOT NULL,
    company_name character varying(255) NOT NULL,
    fiscal_code character varying(255) NOT NULL,
    full_name character varying(255) NOT NULL,
    inserted_date timestamp without time zone NOT NULL,
    status character varying(255) NOT NULL,
    type character varying(255) NOT NULL,
    city character varying(255),
    civic_number character varying(255),
    country character varying(255),
    email character varying(255),
    max_due_date timestamp without time zone NOT NULL,
    min_due_date timestamp without time zone NOT NULL,
    office_name character varying(255),
    phone character varying(255),
    postal_code character varying(255),
    province character varying(255),
    publish_date timestamp without time zone,
    region character varying(255),
    street_name character varying(255),
    validity_date timestamp without time zone,
    version integer NOT NULL DEFAULT 0,
    switch_to_expired boolean NOT NULL DEFAULT false,
    payment_date timestamp without time zone,
    pull boolean NOT NULL DEFAULT true,
    pay_stand_in boolean NOT NULL DEFAULT true,
    service_type character varying(100) NOT NULL DEFAULT 'GPD'::character varying,
    CONSTRAINT payment_position_pkey PRIMARY KEY (id, last_updated_date),
    CONSTRAINT uniquepaymentpos UNIQUE (iupd, organization_fiscal_code, last_updated_date)
) PARTITION BY RANGE (last_updated_date);

-- pg_partman configuration (Interval: 1 month)
SELECT partman.create_parent('apd.payment_position', 'last_updated_date', '1 month');

-- =============================================================================
-- TABLE: PAYMENT_OPTION
-- =============================================================================
CREATE TABLE apd.payment_option (
    id bigint NOT NULL,
    last_updated_date timestamp without time zone NOT NULL,
    payment_position_id bigint NOT NULL,
    organization_fiscal_code character varying(255) NOT NULL,
    iuv character varying(255) NOT NULL,
    nav character varying(255) NOT NULL,
    amount bigint NOT NULL,
    description character varying(255) NOT NULL,
    due_date timestamp without time zone NOT NULL,
    fee bigint NOT NULL,
    status character varying(255) NOT NULL,
    inserted_date timestamp without time zone NOT NULL,
    is_partial_payment boolean NOT NULL,
    fiscal_code character varying(255) NOT NULL DEFAULT 'NA'::character varying,
    full_name character varying(255) NOT NULL DEFAULT 'NA'::character varying,
    type character varying(255) NOT NULL DEFAULT 'F'::character varying,
    flow_reporting_id character varying(255),
    receipt_id character varying(255),
    payment_date timestamp without time zone,
    payment_method character varying(255),
    psp_company character varying(255),
    reporting_date timestamp without time zone,
    retention_date timestamp without time zone,
    notification_fee bigint NOT NULL DEFAULT 0,
    last_updated_date_notification_fee timestamp without time zone,
    street_name character varying(255),
    civic_number character varying(255),
    postal_code character varying(255),
    city character varying(255),
    province character varying(255),
    region character varying(255),
    country character varying(255),
    email character varying(255),
    phone character varying(255),
    send_sync boolean NOT NULL DEFAULT false,
    psp_code character varying(50),
    psp_tax_code character varying(70),
    payment_plan_id text,
    switch_to_expired boolean,
    validity_date timestamp without time zone,
    payment_option_description text,
    CONSTRAINT payment_option_pkey PRIMARY KEY (id, last_updated_date),
    CONSTRAINT uniquepaymentopt UNIQUE (iuv, organization_fiscal_code, last_updated_date),
    CONSTRAINT uniquepaymentoptnav UNIQUE (nav, organization_fiscal_code, last_updated_date),
    CONSTRAINT fk_payment_position_id FOREIGN KEY (payment_position_id, last_updated_date)
        REFERENCES apd.payment_position (id, last_updated_date)
) PARTITION BY RANGE (last_updated_date);

CREATE INDEX idx_payment_position_id ON apd.payment_option(payment_position_id, last_updated_date);
SELECT partman.create_parent('apd.payment_option', 'last_updated_date', '1 month');

-- =============================================================================
-- TABLE: PAYMENT_OPTION_METADATA
-- =============================================================================
CREATE TABLE apd.payment_option_metadata (
    id bigint NOT NULL,
    payment_option_id bigint NOT NULL,
    last_updated_date timestamp without time zone NOT NULL,
    key character varying(140) NOT NULL,
    value character varying(140),
    CONSTRAINT payment_option_metadata_pkey PRIMARY KEY (id, last_updated_date),
    CONSTRAINT fk_payment_option_id FOREIGN KEY (payment_option_id, last_updated_date)
        REFERENCES apd.payment_option (id, last_updated_date)
) PARTITION BY RANGE (last_updated_date);

CREATE INDEX idx_payment_option_metadata_id ON apd.payment_option_metadata(payment_option_id, last_updated_date);
SELECT partman.create_parent('apd.payment_option_metadata', 'last_updated_date', '1 month');

-- =============================================================================
-- TABLE: TRANSFER
-- =============================================================================
CREATE TABLE apd.transfer (
    id bigint NOT NULL,
    payment_option_id bigint NOT NULL,
    last_updated_date timestamp without time zone NOT NULL,
    transfer_id character varying(255) NOT NULL,
    iuv character varying(255) NOT NULL,
    organization_fiscal_code character varying(255) NOT NULL,
    amount bigint NOT NULL,
    category character varying(255) NOT NULL,
    iban character varying(255),
    inserted_date timestamp without time zone NOT NULL,
    status character varying(255) NOT NULL,
    remittance_information character varying(255) NOT NULL,
    postal_iban character varying(255),
    hash_document character varying(255),
    stamp_type character varying(255),
    provincial_residence character varying(255),
    company_name character varying(255),
    CONSTRAINT transfer_pkey PRIMARY KEY (id, last_updated_date),
    CONSTRAINT unique_transfer UNIQUE (iuv, organization_fiscal_code, transfer_id, payment_option_id, last_updated_date),
    CONSTRAINT fk_payment_option_id FOREIGN KEY (payment_option_id, last_updated_date)
        REFERENCES apd.payment_option (id, last_updated_date)
) PARTITION BY RANGE (last_updated_date);

CREATE INDEX idx_transfer_payment_option_id ON apd.transfer(payment_option_id, last_updated_date);
SELECT partman.create_parent('apd.transfer', 'last_updated_date', '1 month');

-- =============================================================================
-- TABLE: TRANSFER_METADATA
-- =============================================================================
CREATE TABLE apd.transfer_metadata (
    id bigint NOT NULL,
    transfer_id bigint NOT NULL,
    last_updated_date timestamp without time zone NOT NULL,
    key character varying(140) NOT NULL,
    value character varying(140),
    CONSTRAINT transfer_metadata_pkey PRIMARY KEY (id, last_updated_date),
    CONSTRAINT fk_transfer_id FOREIGN KEY (transfer_id, last_updated_date)
        REFERENCES apd.transfer (id, last_updated_date)
) PARTITION BY RANGE (last_updated_date);

CREATE INDEX idx_transfer_id_partitioned ON apd.transfer_metadata(transfer_id, last_updated_date);
SELECT partman.create_parent('apd.transfer_metadata', 'last_updated_date', '1 month');
