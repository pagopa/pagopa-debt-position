ALTER TABLE apd.transfer ADD CONSTRAINT unique_transfer UNIQUE USING INDEX unique_transfer;
ALTER TABLE apd.transfer DROP CONSTRAINT uniquetransfer;