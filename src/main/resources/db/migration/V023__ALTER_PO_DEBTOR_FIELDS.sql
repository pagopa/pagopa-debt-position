-- Step 1: Added columns without default to avoid FULL TABLE SCAN
ALTER TABLE payment_option 
            ADD COLUMN IF NOT exists fiscal_code                VARCHAR(255),
            ADD COLUMN IF NOT EXISTS full_name                  VARCHAR(255),
            ADD COLUMN IF NOT EXISTS "type"                     VARCHAR(255),
            ADD COLUMN IF NOT EXISTS street_name                VARCHAR(255),
            ADD COLUMN IF NOT EXISTS civic_number               VARCHAR(255),
            ADD COLUMN IF NOT EXISTS postal_code                VARCHAR(255),
            ADD COLUMN IF NOT EXISTS city                       VARCHAR(255),
            ADD COLUMN IF NOT EXISTS province                   VARCHAR(255),
            ADD COLUMN IF NOT EXISTS region                     VARCHAR(255),
            ADD COLUMN IF NOT EXISTS country                    VARCHAR(255),
            ADD COLUMN IF NOT EXISTS email                      VARCHAR(255),
            ADD COLUMN IF NOT EXISTS phone                      VARCHAR(255);

-- Step 2: Batch update by procedure to minimize lock
CREATE OR replace PROCEDURE update_payment_option_batch(batch_size INT) 
LANGUAGE plpgsql
AS
  $$
  DECLARE
    rows_updated INT;
    total_rows_updated INT := 0;
  BEGIN
    LOOP
      -- Perform update in a standalone transaction
      WITH rows_to_update AS
      (
             SELECT po.payment_position_id,
                    pp.fiscal_code,
                    pp.full_name,
                    pp."type",
                    pp.street_name,
                    pp.civic_number,
                    pp.postal_code,
                    pp.city,
                    pp.province,
                    pp.region,
                    pp.country,
                    pp.email,
                    pp.phone
             FROM   payment_option   AS po
             JOIN   payment_position AS pp
             ON     po.payment_position_id = pp.id
             WHERE  po.fiscal_code IS NULL -- To update only the rows not yet processed
             LIMIT  batch_size )
      UPDATE payment_option AS po
      SET    fiscal_code = rows_to_update.fiscal_code,
             full_name = rows_to_update.full_name,
             "type" = rows_to_update."type",
             street_name = rows_to_update.street_name,
             civic_number = rows_to_update.civic_number,
             postal_code = rows_to_update.postal_code,
             city = rows_to_update.city,
             province = rows_to_update.province,
             region = rows_to_update.region,
             country = rows_to_update.country,
             email = rows_to_update.email,
             phone = rows_to_update.phone
      FROM   rows_to_update
      WHERE  po.payment_position_id = rows_to_update.payment_position_id;
      
      GET diagnostics rows_updated = row_count;
      total_rows_updated := total_rows_updated + rows_updated;
      COMMIT;
      EXIT WHEN rows_updated = 0;
      -- Notify batch completion
      RAISE notice 'Batch block completed: % updated rows.', rows_updated;
    END LOOP;
    ALTER TABLE payment_option 
                ALTER COLUMN fiscal_code SET NOT NULL,
                ALTER COLUMN full_name SET NOT NULL,
                ALTER COLUMN "type" SET DEFAULT 'F',
                ALTER COLUMN "type" SET NOT NULL;
  RAISE NOTICE 'Total rows updated: %', total_rows_updated;
  END $$;
  
  CALL update_payment_option_batch(10000);
  
  DROP PROCEDURE IF EXISTS update_payment_option_batch;