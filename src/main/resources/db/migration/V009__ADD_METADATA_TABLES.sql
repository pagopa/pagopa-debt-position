CREATE SEQUENCE payment_opt_metadata_seq
	INCREMENT BY 1
	MINVALUE 1
	MAXVALUE 9223372036854775807
	START 1
	CACHE 1
	NO CYCLE;
	
CREATE SEQUENCE transfer_metadata_seq
	INCREMENT BY 1
	MINVALUE 1
	MAXVALUE 9223372036854775807
	START 1
	CACHE 1
	NO CYCLE;
	

CREATE TABLE payment_option_metadata (
	id int8 NOT NULL,
	key varchar(140) NOT NULL,
	value varchar(140) NULL,
	payment_option_id int8 NOT NULL,
	CONSTRAINT payment_option_metadata_pkey PRIMARY KEY (id),
	CONSTRAINT uniquepaymentoptmetadata UNIQUE (key, payment_option_id),
	CONSTRAINT fk_payment_option_id FOREIGN KEY (payment_option_id) REFERENCES payment_option(id)
);

CREATE TABLE transfer_metadata (
	id int8 NOT NULL,
	key varchar(140) NOT NULL,
	value varchar(140) NULL,
	transfer_id int8 NOT NULL,
	CONSTRAINT transfer_metadata_pkey PRIMARY KEY (id),
	CONSTRAINT uniquetransfermetadata UNIQUE (key, transfer_id),
	CONSTRAINT fk_transfer_id FOREIGN KEY (transfer_id) REFERENCES transfer(id)
);
