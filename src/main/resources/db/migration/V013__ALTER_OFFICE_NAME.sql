UPDATE payment_position SET office_name = company_name where office_name is NULL;
ALTER TABLE payment_position ALTER COLUMN office_name SET NOT NULL;
