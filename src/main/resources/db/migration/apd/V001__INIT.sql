CREATE SEQUENCE payment_opt_seq
	INCREMENT BY 1
	MINVALUE 1
	MAXVALUE 9223372036854775807
	START 1
	CACHE 1
	NO CYCLE;
	
CREATE SEQUENCE payment_pos_seq
	INCREMENT BY 1
	MINVALUE 1
	MAXVALUE 9223372036854775807
	START 1
	CACHE 1
	NO CYCLE;

CREATE SEQUENCE transfer_seq
	INCREMENT BY 1
	MINVALUE 1
	MAXVALUE 9223372036854775807
	START 1
	CACHE 1
	NO CYCLE;
	
CREATE TABLE payment_position (
	id int8 NOT NULL,
	city varchar(255) NULL,
	civic_number varchar(255) NULL,
	company_name varchar(255) NOT NULL,
	country varchar(255) NULL,
	email varchar(255) NULL,
	fiscal_code varchar(255) NOT NULL,
	full_name varchar(255) NOT NULL,
	inserted_date timestamp NOT NULL,
	iupd varchar(255) NOT NULL,
	last_updated_date timestamp NOT NULL,
	max_due_date timestamp NOT NULL,
	min_due_date timestamp NOT NULL,
	office_name varchar(255) NULL,
	organization_fiscal_code varchar(255) NOT NULL,
	phone varchar(255) NULL,
	postal_code varchar(255) NULL,
	province varchar(255) NULL,
	publish_date timestamp NULL,
	region varchar(255) NULL,
	status varchar(255) NOT NULL,
	street_name varchar(255) NULL,
	"type" varchar(255) NOT NULL,
	validity_date timestamp NULL,
	"version" int4 NOT NULL DEFAULT 0,
	CONSTRAINT payment_position_pkey PRIMARY KEY (id),
	CONSTRAINT uniquepaymentpos UNIQUE (iupd, organization_fiscal_code)
);

CREATE TABLE payment_option (
	id int8 NOT NULL,
	amount int8 NOT NULL,
	description varchar(255) NULL,
	due_date timestamp NOT NULL,
	fee int8 NOT NULL,
	flow_reporting_id varchar(255) NULL,
	receipt_id varchar(255) NULL,
	inserted_date timestamp NOT NULL,
	is_partial_payment bool NOT NULL,
	iuv varchar(255) NOT NULL,
	last_updated_date timestamp NOT NULL,
	organization_fiscal_code varchar(255) NOT NULL,
	payment_date timestamp NULL,
	payment_method varchar(255) NULL,
	psp_company varchar(255) NULL,
	reporting_date timestamp NULL,
	retention_date timestamp NULL,
	status varchar(255) NOT NULL,
	payment_position_id int8 NOT NULL,
	CONSTRAINT payment_option_pkey PRIMARY KEY (id),
	CONSTRAINT uniquepaymentopt UNIQUE (iuv, organization_fiscal_code),
	CONSTRAINT fk_payment_position_id FOREIGN KEY (payment_position_id) REFERENCES payment_position(id)
);


CREATE TABLE transfer (
	id int8 NOT NULL,
	amount int8 NOT NULL,
	category varchar(255) NOT NULL,
	iban varchar(255) NOT NULL,
	transfer_id varchar(255) NOT NULL,
	inserted_date timestamp NOT NULL,
	iuv varchar(255) NOT NULL,
	last_updated_date timestamp NOT NULL,
	organization_fiscal_code varchar(255) NOT NULL,
	postal_iban varchar(255) NULL,
	remittance_information varchar(255) NOT NULL,
	status varchar(255) NOT NULL,
	payment_option_id int8 NOT NULL,
	CONSTRAINT transfer_pkey PRIMARY KEY (id),
	CONSTRAINT uniquetransfer UNIQUE (iuv, organization_fiscal_code, transfer_id),
	CONSTRAINT fk_payment_option_id FOREIGN KEY (payment_option_id) REFERENCES payment_option(id)
);
