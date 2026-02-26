#!/usr/bin/env python3

"""
delete-entities.py

cleanup utility for APD database.

Features
--------
- Bulk deletion using set-based SQL
- Progress logging
- Execution time estimation
- Dry-run support
- Reusable for different cleanup tasks
"""

import argparse
import psycopg2
from psycopg2.extras import execute_values
import logging
import time
import sys


# ---------------------------------------------------------------------
# LOGGING CONFIGURATION
# ---------------------------------------------------------------------

def setup_logging():

    logger = logging.getLogger("cleanup")
    logger.setLevel(logging.INFO)

    formatter = logging.Formatter(
        "%(asctime)s [%(levelname)s] %(message)s"
    )

    console = logging.StreamHandler()
    console.setFormatter(formatter)

    file_handler = logging.FileHandler("cleanup.log")
    file_handler.setFormatter(formatter)

    logger.addHandler(console)
    logger.addHandler(file_handler)

    return logger


# ---------------------------------------------------------------------
# DATABASE CONNECTION
# ---------------------------------------------------------------------

def connect(args):

    return psycopg2.connect(
        host=args.db_host,
        port=args.db_port,
        dbname=args.db_name,
        user=args.db_user,
        password=args.db_password
    )


# ---------------------------------------------------------------------
# ORGANIZATION CODE LOADING
# ---------------------------------------------------------------------

def load_org_codes(args):

    codes = []

    if args.org_codes:
        codes.extend(args.org_codes)

    if args.org_codes_file:
        with open(args.org_codes_file) as f:
            for line in f:
                line = line.strip()
                if line:
                    codes.append(line)

    if not codes:
        raise Exception("No organization codes provided")

    return list(set(codes))


# ---------------------------------------------------------------------
# TEMP TABLE CREATION (automatically deleted when the connection closes)
# ---------------------------------------------------------------------

def create_temp_tables(cur):

    cur.execute("""
    CREATE TEMP TABLE tmp_org_codes (
        organization_fiscal_code varchar PRIMARY KEY
    );
    """)

    cur.execute("""
    CREATE TEMP TABLE tmp_target_payment_position (
        payment_position_id bigint PRIMARY KEY
    );
    """)


# ---------------------------------------------------------------------
# INSERT ORG CODES
# ---------------------------------------------------------------------

def insert_org_codes(cur, codes):

    execute_values(
        cur,
        "INSERT INTO tmp_org_codes VALUES %s",
        [(c,) for c in codes]
    )


# ---------------------------------------------------------------------
# FREEZE TARGET SET
# ---------------------------------------------------------------------

def freeze_targets(cur, args):

    sql = """
    INSERT INTO tmp_target_payment_position
    SELECT pp.id
    FROM apd.payment_position pp
    JOIN tmp_org_codes t
      ON t.organization_fiscal_code = pp.organization_fiscal_code
    WHERE pp.service_type = %s
      AND pp.status = %s
    """

    if not args.allow_paid:
        sql += " AND pp.payment_date IS NULL"

    cur.execute(sql, (args.service_type, args.payment_position_status))


# ---------------------------------------------------------------------
# COUNT TARGET RECORDS
# ---------------------------------------------------------------------

def count_records(cur):

    queries = {
        "payment_position": """
            SELECT count(*)
            FROM apd.payment_position pp
            JOIN tmp_target_payment_position t
            ON pp.id = t.payment_position_id
        """,

        "payment_option": """
            SELECT count(*)
            FROM apd.payment_option po
            JOIN tmp_target_payment_position t
            ON po.payment_position_id = t.payment_position_id
        """,

        "transfer": """
            SELECT count(*)
            FROM apd.transfer tr
            JOIN apd.payment_option po
            ON po.id = tr.payment_option_id
            JOIN tmp_target_payment_position t
            ON po.payment_position_id = t.payment_position_id
        """
    }

    results = {}

    for name, query in queries.items():
        cur.execute(query)
        results[name] = cur.fetchone()[0]

    return results


# ---------------------------------------------------------------------
# DELETE FUNCTIONS
# ---------------------------------------------------------------------

def delete_transfer_metadata(cur):

    cur.execute("""
    DELETE FROM apd.transfer_metadata tm
    USING apd.transfer tr,
          apd.payment_option po,
          tmp_target_payment_position t
    WHERE tr.id = tm.transfer_id
    AND po.id = tr.payment_option_id
    AND po.payment_position_id = t.payment_position_id
    """)


def delete_transfer(cur):

    cur.execute("""
    DELETE FROM apd.transfer tr
    USING apd.payment_option po,
          tmp_target_payment_position t
    WHERE po.id = tr.payment_option_id
    AND po.payment_position_id = t.payment_position_id
    """)


def delete_payment_option_metadata(cur):

    cur.execute("""
    DELETE FROM apd.payment_option_metadata pom
    USING apd.payment_option po,
          tmp_target_payment_position t
    WHERE pom.payment_option_id = po.id
    AND po.payment_position_id = t.payment_position_id
    """)


def delete_payment_option(cur):

    cur.execute("""
    DELETE FROM apd.payment_option po
    USING tmp_target_payment_position t
    WHERE po.payment_position_id = t.payment_position_id
    """)


def delete_payment_position(cur):

    cur.execute("""
    DELETE FROM apd.payment_position pp
    USING tmp_target_payment_position t
    WHERE pp.id = t.payment_position_id
    """)


# ---------------------------------------------------------------------
# EXECUTION TIMER
# ---------------------------------------------------------------------

def timer(start):

    return round(time.time() - start, 2)


# ---------------------------------------------------------------------
# MAIN CLEANUP FLOW
# ---------------------------------------------------------------------

def run_cleanup(args):

    logger = setup_logging()

    start_total = time.time()

    logger.info("Starting cleanup process")

    conn = connect(args)

    try:

        with conn.cursor() as cur:

            logger.info("Creating temporary tables")
            create_temp_tables(cur)

            org_codes = load_org_codes(args)

            logger.info(f"Loaded {len(org_codes)} organization codes")

            insert_org_codes(cur, org_codes)

            logger.info("Freezing payment_position target set")

            freeze_targets(cur, args)

            cur.execute("SELECT count(*) FROM tmp_target_payment_position")
            target_count = cur.fetchone()[0]

            logger.info(f"Payment positions selected: {target_count}")

            if target_count == 0:
                logger.info("Nothing to delete")
                return

            logger.info("Counting dependent records")

            counts = count_records(cur)

            for table, value in counts.items():
                logger.info(f"{table}: {value}")

            if args.dry_run:

                logger.info("Dry-run enabled: rollback will be executed")
                conn.rollback()
                return

            phase = time.time()

            logger.info("Deleting transfer_metadata")
            delete_transfer_metadata(cur)
            logger.info(f"Completed in {timer(phase)} sec")

            phase = time.time()

            logger.info("Deleting transfer")
            delete_transfer(cur)
            logger.info(f"Completed in {timer(phase)} sec")

            phase = time.time()

            logger.info("Deleting payment_option_metadata")
            delete_payment_option_metadata(cur)
            logger.info(f"Completed in {timer(phase)} sec")

            phase = time.time()

            logger.info("Deleting payment_option")
            delete_payment_option(cur)
            logger.info(f"Completed in {timer(phase)} sec")

            phase = time.time()

            logger.info("Deleting payment_position")
            delete_payment_position(cur)
            logger.info(f"Completed in {timer(phase)} sec")

            conn.commit()

            logger.info("Cleanup committed successfully")

    except Exception as e:

        conn.rollback()

        logger.error("Cleanup failed")
        logger.error(e)

        sys.exit(1)

    finally:

        conn.close()

    logger.info(f"Total execution time: {timer(start_total)} seconds")


# ---------------------------------------------------------------------
# CLI ARGUMENT PARSER
# ---------------------------------------------------------------------

def parse_args():

    parser = argparse.ArgumentParser()

    parser.add_argument("--db-host", required=True)
    parser.add_argument("--db-port", default=5432)
    parser.add_argument("--db-name", required=True)
    parser.add_argument("--db-user", required=True)
    parser.add_argument("--db-password", required=True)

    parser.add_argument("--org-codes", nargs="*")
    parser.add_argument("--org-codes-file")

    parser.add_argument("--service-type", default="WISP")
    parser.add_argument("--payment-position-status", default="VALID")

    parser.add_argument("--allow-paid", action="store_true")

    parser.add_argument("--dry-run", action="store_true")

    return parser.parse_args()


# ---------------------------------------------------------------------

if __name__ == "__main__":

    args = parse_args()

    run_cleanup(args)