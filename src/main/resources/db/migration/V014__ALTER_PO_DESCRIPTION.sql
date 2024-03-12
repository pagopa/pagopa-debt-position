UPDATE payment_option SET description = 'NA' where description is NULL;
ALTER TABLE payment_option ALTER COLUMN description SET NOT NULL;
