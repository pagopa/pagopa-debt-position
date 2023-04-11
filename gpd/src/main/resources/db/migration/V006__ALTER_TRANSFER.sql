ALTER TABLE transfer ADD COLUMN "hash_document" varchar(255) NULL;
ALTER TABLE transfer ADD COLUMN "stamp_type" varchar(255) NULL;
ALTER TABLE transfer ADD COLUMN "provincial_residence" varchar(255) NULL;
ALTER TABLE transfer ALTER COLUMN "iban" DROP NOT NULL;
