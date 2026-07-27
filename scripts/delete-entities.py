#!/usr/bin/env python3

"""
delete-entities.py

Cleanup utility for APD database.

Features
--------
- Bulk deletion using set-based SQL
- Progress logging
- Execution time estimation
- Dry-run support
- Optional retention filter based on payment_position.last_updated_date
- Optional batch deletion mode to avoid long-running transactions
- Reusable for different cleanup tasks
"""

import argparse
import logging
import sys
import time
from datetime import timedelta

import psycopg2
from psycopg2.extras import execute_values


# ---------------------------------------------------------------------
# LOGGING CONFIGURATION
# ---------------------------------------------------------------------

def setup_logging():
    """
    Configure the logger used by the script.

    Logs are written both to:
    - console
    - cleanup.log file
    """

    logger = logging.getLogger("cleanup")
    logger.setLevel(logging.INFO)
    logger.propagate = False

    if logger.handlers:
        logger.handlers.clear()

    formatter = logging.Formatter(
        "%(asctime)s [%(levelname)s] %(message)s"
    )

    console = logging.StreamHandler(sys.stdout)
    console.setFormatter(formatter)

    file_handler = logging.FileHandler("cleanup.log", encoding="utf-8")
    file_handler.setFormatter(formatter)

    logger.addHandler(console)
    logger.addHandler(file_handler)

    return logger


# ---------------------------------------------------------------------
# DATABASE CONNECTION
# ---------------------------------------------------------------------

def connect(args):
    """
    Create a PostgreSQL connection using CLI parameters.
    """

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
    """
    Load organization fiscal codes from:
    - --org-codes
    - --org-codes-file

    Duplicates are removed using python dict feature.
    """

    codes = []

    if args.org_codes:
        codes.extend(args.org_codes)

    if args.org_codes_file:
        with open(args.org_codes_file, encoding="utf-8") as f:
            for line in f:
                line = line.strip()
                if line:
                    codes.append(line)

    if not codes:
        raise Exception("No organization codes provided")

    return list(dict.fromkeys(codes))


# ---------------------------------------------------------------------
# TEMP TABLE CREATION (automatically deleted when the connection closes)
# ---------------------------------------------------------------------

def create_temp_tables(cur):
    """
    Create session-scoped temporary tables.

    These tables are automatically dropped when the DB session ends.
    """

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
    """
    Bulk insert organization fiscal codes into temp table.
    """

    execute_values(
        cur,
        "INSERT INTO tmp_org_codes VALUES %s",
        [(c,) for c in codes]
    )


# ---------------------------------------------------------------------
# TIME / RETENTION HELPERS
# ---------------------------------------------------------------------

def get_db_current_timestamp(cur):
    """
    Read the current timestamp from the database server.
    This avoid discrepancies between the PC clock and the server clock.
    """

    cur.execute("SELECT CURRENT_TIMESTAMP")
    return cur.fetchone()[0]


def compute_cutoff_timestamp(db_now, retention_days):
    """
    Compute the retention cutoff timestamp.

    Records with last_updated_date >= cutoff_timestamp will be excluded.
    """
    return db_now - timedelta(days=retention_days)


# ---------------------------------------------------------------------
# FREEZE TARGET SET
# ---------------------------------------------------------------------

def freeze_targets(cur, args, cutoff_timestamp=None):
    """
    Freeze the target payment_position IDs into a temp table.

    Optional filters:
    - exclude paid positions unless --allow-paid is used
    - exclude recent positions using retention based on last_updated_date
    """

    sql = """
    INSERT INTO tmp_target_payment_position
    SELECT pp.id
    FROM apd.payment_position pp
    JOIN tmp_org_codes t
    ON t.organization_fiscal_code = pp.organization_fiscal_code
    WHERE pp.service_type = %s
    AND pp.status = %s
    """

    params = [args.service_type, args.payment_position_status]

    if not args.allow_paid:
        sql += " AND pp.payment_date IS NULL"

    if cutoff_timestamp is not None:
        sql += " AND pp.last_updated_date < %s"
        params.append(cutoff_timestamp)

    cur.execute(sql, params)


# ---------------------------------------------------------------------
# COUNT TARGET RECORDS
# ---------------------------------------------------------------------

def count_records(cur):
    """
    Count how many dependent rows are currently associated to the
    payment positions.
    """

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
        "payment_option_metadata": """
            SELECT count(*)
            FROM apd.payment_option_metadata pom
            JOIN apd.payment_option po
            ON pom.payment_option_id = po.id
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
        """,
        "transfer_metadata": """
            SELECT count(*)
            FROM apd.transfer_metadata tm
            JOIN apd.transfer tr
            ON tr.id = tm.transfer_id
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


def count_remaining_targets(cur):
    """
    Count how many payment_position IDs are still present
    in the target table.
    """
    cur.execute("SELECT count(*) FROM tmp_target_payment_position")
    return cur.fetchone()[0]


# ---------------------------------------------------------------------
# FULL DELETE FUNCTIONS (single-transaction mode)
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
    return cur.rowcount


def delete_transfer(cur):
    cur.execute("""
    DELETE FROM apd.transfer tr
    USING apd.payment_option po,
          tmp_target_payment_position t
    WHERE po.id = tr.payment_option_id
    AND po.payment_position_id = t.payment_position_id
    """)
    return cur.rowcount


def delete_payment_option_metadata(cur):
    cur.execute("""
    DELETE FROM apd.payment_option_metadata pom
    USING apd.payment_option po,
          tmp_target_payment_position t
    WHERE pom.payment_option_id = po.id
    AND po.payment_position_id = t.payment_position_id
    """)
    return cur.rowcount


def delete_payment_option(cur):
    cur.execute("""
    DELETE FROM apd.payment_option po
    USING tmp_target_payment_position t
    WHERE po.payment_position_id = t.payment_position_id
    """)
    return cur.rowcount


def delete_payment_position(cur):
    cur.execute("""
    DELETE FROM apd.payment_position pp
    USING tmp_target_payment_position t
    WHERE pp.id = t.payment_position_id
    """)
    return cur.rowcount


# ---------------------------------------------------------------------
# BATCH DELETE FUNCTIONS
# ---------------------------------------------------------------------

def build_batch_subquery():
    """
    Subquery used to select the current batch of payment positions.
    Ordering by payment_position_id makes the batching deterministic.
    """
    return """
        SELECT payment_position_id
        FROM tmp_target_payment_position
        ORDER BY payment_position_id
        LIMIT %s
    """


def delete_transfer_metadata_batch(cur, batch_size):
    cur.execute(f"""
    DELETE FROM apd.transfer_metadata tm
    USING apd.transfer tr,
          apd.payment_option po
    WHERE tr.id = tm.transfer_id
    AND po.id = tr.payment_option_id
    AND po.payment_position_id IN ({build_batch_subquery()})
    """, (batch_size,))
    return cur.rowcount


def delete_transfer_batch(cur, batch_size):
    cur.execute(f"""
    DELETE FROM apd.transfer tr
    USING apd.payment_option po
    WHERE po.id = tr.payment_option_id
    AND po.payment_position_id IN ({build_batch_subquery()})
    """, (batch_size,))
    return cur.rowcount


def delete_payment_option_metadata_batch(cur, batch_size):
    cur.execute(f"""
    DELETE FROM apd.payment_option_metadata pom
    USING apd.payment_option po
    WHERE pom.payment_option_id = po.id
    AND po.payment_position_id IN ({build_batch_subquery()})
    """, (batch_size,))
    return cur.rowcount


def delete_payment_option_batch(cur, batch_size):
    cur.execute(f"""
    DELETE FROM apd.payment_option po
    WHERE po.payment_position_id IN ({build_batch_subquery()})
    """, (batch_size,))
    return cur.rowcount


def delete_payment_position_batch(cur, batch_size):
    cur.execute(f"""
    DELETE FROM apd.payment_position pp
    WHERE pp.id IN ({build_batch_subquery()})
    """, (batch_size,))
    return cur.rowcount


def consume_processed_batch(cur, batch_size):
    """
    Remove the processed payment_position IDs from the temp target table
    after the batch has been committed.
    This is what allows the next batch to move forward.
    """
    cur.execute(f"""
    DELETE FROM tmp_target_payment_position t
    WHERE t.payment_position_id IN ({build_batch_subquery()})
    """, (batch_size,))
    return cur.rowcount


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
    logger.info("Parameters | service_type=%s | payment_position_status=%s | allow_paid=%s | dry_run=%s",
                args.service_type, args.payment_position_status, args.allow_paid, args.dry_run)

    if args.retention_days is not None:
        logger.info("Retention filter requested | retention_days=%s", args.retention_days)
    else:
        logger.info("Retention filter disabled")

    if args.batch_size is not None:
        logger.info("Batch delete mode enabled | batch_size=%s", args.batch_size)
    else:
        logger.info("Single transaction delete mode enabled")

    conn = connect(args)

    try:
        with conn.cursor() as cur:
            logger.info("Creating temporary tables")
            create_temp_tables(cur)

            org_codes = load_org_codes(args)
            logger.info("Loaded %s organization codes", len(org_codes))

            insert_org_codes(cur, org_codes)

            cutoff_timestamp = None
            if args.retention_days is not None:
                db_now = get_db_current_timestamp(cur)
                cutoff_timestamp = compute_cutoff_timestamp(db_now, args.retention_days)
                logger.info(
                    "Retention filter active | database_now=%s | cutoff_date=%s | extraction_rule=last_updated_date < cutoff_date",
                    db_now, cutoff_timestamp
                )

            logger.info("Freezing payment_position target set")
            freeze_targets(cur, args, cutoff_timestamp=cutoff_timestamp)

            target_count = count_remaining_targets(cur)
            logger.info("Payment positions selected: %s", target_count)

            if target_count == 0:
                logger.info("Nothing to delete")
                conn.rollback()
                return

            logger.info("Counting dependent records")
            counts = count_records(cur)
            for table, value in counts.items():
                logger.info("%s: %s", table, value)

            if args.dry_run:
                logger.info("Dry-run enabled: rollback will be executed, no data will be deleted")
                conn.rollback()
                logger.info("Dry-run completed successfully")
                return

            # ---------------------------------------------------------
            # SINGLE TRANSACTION MODE
            # ---------------------------------------------------------
            if args.batch_size is None:
                phase = time.time()
                logger.info("Deleting transfer_metadata")
                deleted = delete_transfer_metadata(cur)
                logger.info("Deleted transfer_metadata=%s | completed in %s sec", deleted, timer(phase))

                phase = time.time()
                logger.info("Deleting transfer")
                deleted = delete_transfer(cur)
                logger.info("Deleted transfer=%s | completed in %s sec", deleted, timer(phase))

                phase = time.time()
                logger.info("Deleting payment_option_metadata")
                deleted = delete_payment_option_metadata(cur)
                logger.info("Deleted payment_option_metadata=%s | completed in %s sec", deleted, timer(phase))

                phase = time.time()
                logger.info("Deleting payment_option")
                deleted = delete_payment_option(cur)
                logger.info("Deleted payment_option=%s | completed in %s sec", deleted, timer(phase))

                phase = time.time()
                logger.info("Deleting payment_position")
                deleted = delete_payment_position(cur)
                logger.info("Deleted payment_position=%s | completed in %s sec", deleted, timer(phase))

                conn.commit()
                logger.info("Cleanup committed successfully")

            # ---------------------------------------------------------
            # BATCH MODE
            # ---------------------------------------------------------
            else:
                batch_no = 0

                while True:
                    remaining_before = count_remaining_targets(cur)

                    if remaining_before == 0:
                        break

                    batch_no += 1
                    batch_start = time.time()

                    logger.info(
                        "Starting batch %s | configured_batch_size=%s | remaining_targets_before_batch=%s",
                        batch_no, args.batch_size, remaining_before
                    )

                    phase = time.time()
                    deleted_tm = delete_transfer_metadata_batch(cur, args.batch_size)
                    logger.info(
                        "Batch %s | deleted transfer_metadata=%s | duration=%s sec",
                        batch_no, deleted_tm, timer(phase)
                    )

                    phase = time.time()
                    deleted_tr = delete_transfer_batch(cur, args.batch_size)
                    logger.info(
                        "Batch %s | deleted transfer=%s | duration=%s sec",
                        batch_no, deleted_tr, timer(phase)
                    )

                    phase = time.time()
                    deleted_pom = delete_payment_option_metadata_batch(cur, args.batch_size)
                    logger.info(
                        "Batch %s | deleted payment_option_metadata=%s | duration=%s sec",
                        batch_no, deleted_pom, timer(phase)
                    )

                    phase = time.time()
                    deleted_po = delete_payment_option_batch(cur, args.batch_size)
                    logger.info(
                        "Batch %s | deleted payment_option=%s | duration=%s sec",
                        batch_no, deleted_po, timer(phase)
                    )

                    phase = time.time()
                    deleted_pp = delete_payment_position_batch(cur, args.batch_size)
                    logger.info(
                        "Batch %s | deleted payment_position=%s | duration=%s sec",
                        batch_no, deleted_pp, timer(phase)
                    )

                    # Remove processed IDs from the set only after
                    # all related deletes for the batch have been executed.
                    consumed = consume_processed_batch(cur, args.batch_size)

                    conn.commit()

                    remaining_after = count_remaining_targets(cur)

                    logger.info(
                        "Completed batch %s | consumed_target_ids=%s | remaining_targets_after_batch=%s | batch_duration=%s sec",
                        batch_no, consumed, remaining_after, timer(batch_start)
                    )

                logger.info("Batch cleanup committed successfully")

    except Exception as e:
        conn.rollback()
        logger.exception("Cleanup failed: %s", e)
        sys.exit(1)

    finally:
        conn.close()

    logger.info("Total execution time: %s seconds", timer(start_total))


# ---------------------------------------------------------------------
# CLI ARGUMENT PARSER
# ---------------------------------------------------------------------

def parse_args():
    parser = argparse.ArgumentParser()

    parser.add_argument("--db-host", required=True)
    parser.add_argument("--db-port", default=5432, type=int)
    parser.add_argument("--db-name", required=True)
    parser.add_argument("--db-user", required=True)
    parser.add_argument("--db-password", required=True)

    parser.add_argument("--org-codes", nargs="*")
    parser.add_argument("--org-codes-file")

    parser.add_argument("--service-type", default="WISP")
    parser.add_argument("--payment-position-status", default="VALID")

    parser.add_argument("--allow-paid", action="store_true")
    parser.add_argument("--dry-run", action="store_true")

    # Retention feature:
    # if provided, only records with last_updated_date older than the cutoff
    # will be included in the target set.
    parser.add_argument(
        "--retention-days",
        type=int,
        default=None,
        help="Exclude payment positions updated in the last N days based on last_updated_date"
    )

    # Batch delete feature:
    # if provided, deletion will be executed in chunks of this size.
    parser.add_argument(
        "--batch-size",
        type=int,
        default=None,
        help="Delete records in batches of N payment positions per transaction"
    )

    args = parser.parse_args()

    if args.retention_days is not None and args.retention_days < 0:
        parser.error("--retention-days must be >= 0")

    if args.batch_size is not None and args.batch_size <= 0:
        parser.error("--batch-size must be > 0")

    return args


# ---------------------------------------------------------------------

if __name__ == "__main__":
    args = parse_args()
    run_cleanup(args)