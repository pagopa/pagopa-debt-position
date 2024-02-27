ALTER TABLE payment_option ADD COLUMN nav varchar(255);

UPDATE payment_option SET nav = '3'||iuv;

ALTER TABLE payment_option ALTER COLUMN nav SET NOT NULL;

ALTER TABLE payment_option ADD CONSTRAINT uniquepaymentoptnav UNIQUE (nav, organization_fiscal_code);