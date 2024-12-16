-- Step 1: Added columns without default to avoid FULL TABLE SCAN
ALTER TABLE payment_option 
ADD COLUMN IF NOT EXISTS fiscal_code varchar(255),
ADD COLUMN IF NOT EXISTS full_name varchar(255),
ADD COLUMN IF NOT EXISTS "type" varchar(255),
ADD COLUMN IF NOT EXISTS street_name varchar(255),
ADD COLUMN IF NOT EXISTS civic_number varchar(255),
ADD COLUMN IF NOT EXISTS postal_code varchar(255),
ADD COLUMN IF NOT EXISTS city varchar(255),
ADD COLUMN IF NOT EXISTS province varchar(255),
ADD COLUMN IF NOT EXISTS region varchar(255),
ADD COLUMN IF NOT EXISTS country varchar(255),
ADD COLUMN IF NOT EXISTS email varchar(255),
ADD COLUMN IF NOT EXISTS phone varchar(255);

-- Step 2: Batch update to minimize lock
DO $$
DECLARE
    batch_size INT := 10000; -- Batch size
    rows_updated INT;
BEGIN
    LOOP
        WITH rows_to_update AS (
            SELECT po.payment_position_id, pp.fiscal_code, pp.full_name, pp."type", 
                   pp.street_name, pp.civic_number, pp.postal_code, pp.city, 
                   pp.province, pp.region, pp.country, pp.email, pp.phone
            FROM payment_option AS po
            JOIN payment_position AS pp 
              ON po.payment_position_id = pp.id
            WHERE po.fiscal_code IS NULL -- To update only the rows not yet processed
            LIMIT batch_size
        )
        UPDATE payment_option AS po
        SET fiscal_code = rows_to_update.fiscal_code,
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
        FROM rows_to_update
        WHERE po.payment_position_id = rows_to_update.payment_position_id;

        GET DIAGNOSTICS rows_updated = ROW_COUNT;

        EXIT WHEN rows_updated = 0;
    END LOOP;
END $$;

-- Step 3: Set the columns that must always be filled to NOT NULL
ALTER TABLE payment_option
ALTER COLUMN fiscal_code SET NOT NULL,
ALTER COLUMN full_name SET NOT NULL,
ALTER COLUMN "type" SET DEFAULT 'F',
ALTER COLUMN "type" SET NOT NULL;
