# delete-entities.py

Cleanup utility for the APD database.

This script is designed to delete `payment_position` records and all dependent rows in the correct foreign-key order, using **set-based SQL operations**.

It is meant to be reusable for future cleanup activities with similar characteristics.

---

## What the script does

The script deletes data in this order:

1. `transfer_metadata`
2. `transfer`
3. `payment_option_metadata`
4. `payment_option`
5. `payment_position`

This order is required to respect foreign key dependencies.

The script first builds a temporary target set of `payment_position.id` values and then uses that frozen set for all subsequent delete operations.

---

## Main features

- Better performance through **set-based deletes**
- Uses **temporary tables** to freeze the target scope
- Supports **dry-run mode** for safe validation
- Supports organization codes passed:
  - directly from command line
  - through a text file
- Includes **process logging**
- Measures **execution time per phase** and total runtime
- Supports **retention filtering** based on `payment_position.last_updated_date`
- Supports **batch deletion mode** to avoid long-running transactions
- Reusable for future cleanup tasks

---

## Functional selection criteria

By default, the script targets `payment_position` rows matching:

- `organization_fiscal_code` in the provided list
- `service_type = 'WISP'` (unless overridden)
- `status = 'VALID'` (unless overridden)
- `payment_date IS NULL`

If you explicitly pass `--allow-paid`, the filter on `payment_date IS NULL` is removed.

If you pass `--retention-days N`, the script will include only rows with:

- `last_updated_date < current_timestamp - N days`

This means more recent positions are excluded from the cleanup scope.

---

## Prerequisites

- Python
- Python package:

```bash
pip install psycopg2-binary
```

---

## Command line parameters

### Required database parameters

- `--db-host` : database host
- `--db-port` : database port, default `5432`
- `--db-name` : database name
- `--db-user` : database user
- `--db-password` : database password

### Target scope parameters

- `--org-codes` : list of organization fiscal codes passed directly on CLI
- `--org-codes-file` : text file containing one organization fiscal code per line

You can use either one, or both.

### Optional filtering parameters

- `--service-type` : default `WISP`
- `--payment-position-status` : default `VALID`
- `--allow-paid` : include rows with `payment_date IS NOT NULL`
- `--retention-days` : exclude payment positions updated in the last `N` days, based on `last_updated_date`

### Execution mode parameters

- `--dry-run` : simulate the cleanup without deleting data
- `--batch-size` : delete records in batches of `N` payment positions per transaction

---

## How dry-run works

Dry-run mode is the safest way to validate the cleanup before running the real deletion.

When `--dry-run` is used, the script:

1. connects to the database
2. creates temporary tables
3. loads the organization codes
4. computes the retention cutoff date, if requested
5. freezes the target `payment_position` IDs
6. counts how many target rows and dependent rows are involved
7. writes process logs
8. performs a **rollback instead of commit**

With dry-run mode:

- **no data is deleted**
- you can validate the exact scope of the cleanup
- you can estimate runtime and impact before the real execution

---

## Retention mode

Retention mode allows you to exclude the most recent records from the cleanup.

Example:

```bash
--retention-days 30
```

This means that only rows with:

```text
last_updated_date < current_timestamp - 30 days
```

will be selected.

When retention mode is active, the log shows:

- the value passed via `--retention-days`
- the database current timestamp
- the computed cutoff date
- the extraction rule applied

Example log:

```text
Retention filter requested | retention_days=30
Retention filter active | database_now=2026-03-13 10:15:00+00:00 | cutoff_date=2026-02-11 10:15:00+00:00 | extraction_rule=last_updated_date < cutoff_date
```

---

## Batch deletion mode

Batch mode is useful when the cleanup involves a very large number of rows.

Instead of keeping a single large transaction open, the script processes the frozen target set in smaller chunks.

Example:

```bash
--batch-size 1000
```

This means:

- up to 1000 `payment_position` rows are processed per batch
- child rows are deleted first
- parent rows are deleted last
- each batch is committed independently

This reduces:

- transaction duration
- lock time
- rollback cost in case of failure

When batch mode is active, the log shows for each batch:

- batch number
- configured batch size
- remaining target rows before the batch
- deleted rows per table
- batch duration
- remaining target rows after the batch

Example log:

```text
Starting batch 1 | configured_batch_size=1000 | remaining_targets_before_batch=4280
Batch 1 | deleted transfer_metadata=5000 | duration=0.84 sec
Batch 1 | deleted transfer=3200 | duration=0.40 sec
Batch 1 | deleted payment_option_metadata=1000 | duration=0.16 sec
Batch 1 | deleted payment_option=1000 | duration=0.11 sec
Batch 1 | deleted payment_position=1000 | duration=0.09 sec
Completed batch 1 | consumed_target_ids=1000 | remaining_targets_after_batch=3280 | batch_duration=1.82 sec
```

---

## Logging

The script logs:

- start of execution
- execution parameters
- number of organization codes loaded
- retention parameters and computed cutoff date
- number of target `payment_position` rows
- counts of dependent rows
- start/end of each delete phase
- start/end of each batch in batch mode
- execution time per phase
- batch duration
- total execution time
- any exception and rollback

Logs are written:

- to console
- to file `cleanup.log`

---

## Basic usage examples

### 1. Dry-run using organization codes passed on command line

```bash
python delete-entities.py \
  --db-host <DB_HOST> \
  --db-port 5432 \
  --db-name <DB_NAME> \
  --db-user <DB_USER> \
  --db-password '<DB_PASSWORD>' \
  --org-codes 00141660225 00146270228 \
  --dry-run
```

### 2. Real execution using organization codes passed on command line

```bash
python delete-entities.py \
  --db-host <DB_HOST> \
  --db-port 5432 \
  --db-name <DB_NAME> \
  --db-user <DB_USER> \
  --db-password '<DB_PASSWORD>' \
  --org-codes 00141660225 00146270228
```

### 3. Dry-run using a file with organization codes

Create a file, for example `org_codes.txt`:

```text
00141660225
00146270228
00303060222
```

Then run:

```bash
python delete-entities.py \
  --db-host <DB_HOST> \
  --db-port 5432 \
  --db-name <DB_NAME> \
  --db-user <DB_USER> \
  --db-password '<DB_PASSWORD>' \
  --org-codes-file org_codes.txt \
  --dry-run
```

### 4. Real execution using a file with organization codes

```bash
python delete-entities.py \
  --db-host <DB_HOST> \
  --db-port 5432 \
  --db-name <DB_NAME> \
  --db-user <DB_USER> \
  --db-password '<DB_PASSWORD>' \
  --org-codes-file org_codes.txt
```

### 5. Using different functional filters

Example: delete rows with another service type and another status.

```bash
python delete-entities.py \
  --db-host <DB_HOST> \
  --db-port 5432 \
  --db-name <DB_NAME> \
  --db-user <DB_USER> \
  --db-password '<DB_PASSWORD>' \
  --org-codes-file org_codes.txt \
  --service-type XYZ \
  --payment-position-status CREATED \
  --dry-run
```

### 6. Including paid positions

By default, paid positions are excluded.

To include them explicitly:

```bash
python delete-entities.py \
  --db-host <DB_HOST> \
  --db-port 5432 \
  --db-name <DB_NAME> \
  --db-user <DB_USER> \
  --db-password '<DB_PASSWORD>' \
  --org-codes-file org_codes.txt \
  --allow-paid \
  --dry-run
```

### 7. Dry-run with retention

```bash
python delete-entities.py \
  --db-host <DB_HOST> \
  --db-port 5432 \
  --db-name <DB_NAME> \
  --db-user <DB_USER> \
  --db-password '<DB_PASSWORD>' \
  --org-codes-file org_codes.txt \
  --retention-days 30 \
  --dry-run
```

### 8. Real execution with retention

```bash
python delete-entities.py \
  --db-host <DB_HOST> \
  --db-port 5432 \
  --db-name <DB_NAME> \
  --db-user <DB_USER> \
  --db-password '<DB_PASSWORD>' \
  --org-codes-file org_codes.txt \
  --retention-days 30
```

### 9. Real execution with batch mode

```bash
python delete-entities.py \
  --db-host <DB_HOST> \
  --db-port 5432 \
  --db-name <DB_NAME> \
  --db-user <DB_USER> \
  --db-password '<DB_PASSWORD>' \
  --org-codes-file org_codes.txt \
  --batch-size 1000
```

### 10. Real execution with both retention and batch mode

```bash
python delete-entities.py \
  --db-host <DB_HOST> \
  --db-port 5432 \
  --db-name <DB_NAME> \
  --db-user <DB_USER> \
  --db-password '<DB_PASSWORD>' \
  --org-codes-file org_codes.txt \
  --retention-days 30 \
  --batch-size 1000
```

---

## Example output

### Dry-run execution example

```text
2026-03-09 16:52:28,585 [INFO] Starting cleanup process
2026-03-09 16:52:28,586 [INFO] Parameters | service_type=WISP | payment_position_status=VALID | allow_paid=False | dry_run=True
2026-03-09 16:52:28,995 [INFO] Creating temporary tables
2026-03-09 16:52:29,108 [INFO] Loaded 1 organization codes
2026-03-09 16:52:29,144 [INFO] Retention filter requested | retention_days=30
2026-03-09 16:52:29,145 [INFO] Retention filter active | database_now=2026-03-09 16:52:29.145000+00:00 | cutoff_date=2026-02-07 16:52:29.145000+00:00 | extraction_rule=last_updated_date < cutoff_date
2026-03-09 16:52:29,214 [INFO] Freezing payment_position target set
2026-03-09 16:52:29,260 [INFO] Payment positions selected: 360
2026-03-09 16:52:29,261 [INFO] Counting dependent records
2026-03-09 16:52:29,317 [INFO] payment_position: 360
2026-03-09 16:52:29,318 [INFO] payment_option: 360
2026-03-09 16:52:29,318 [INFO] payment_option_metadata: 360
2026-03-09 16:52:29,318 [INFO] transfer: 570
2026-03-09 16:52:29,319 [INFO] transfer_metadata: 570
2026-03-09 16:52:29,319 [INFO] Dry-run enabled: rollback will be executed, no data will be deleted
2026-03-09 16:52:29,320 [INFO] Dry-run completed successfully
```

### Real execution example in single transaction mode

```text
2026-03-09 16:57:53,614 [INFO] Starting cleanup process
2026-03-09 16:57:53,615 [INFO] Parameters | service_type=WISP | payment_position_status=VALID | allow_paid=False | dry_run=False
2026-03-09 16:57:54,041 [INFO] Creating temporary tables
2026-03-09 16:57:54,165 [INFO] Loaded 1 organization codes
2026-03-09 16:57:54,211 [INFO] Freezing payment_position target set
2026-03-09 16:57:54,300 [INFO] Payment positions selected: 360
2026-03-09 16:57:54,301 [INFO] Counting dependent records
2026-03-09 16:57:54,418 [INFO] payment_position: 360
2026-03-09 16:57:54,419 [INFO] payment_option: 360
2026-03-09 16:57:54,419 [INFO] payment_option_metadata: 360
2026-03-09 16:57:54,419 [INFO] transfer: 570
2026-03-09 16:57:54,420 [INFO] transfer_metadata: 570
2026-03-09 16:57:54,420 [INFO] Deleting transfer_metadata
2026-03-09 16:57:54,463 [INFO] Deleted transfer_metadata=570 | completed in 0.04 sec
2026-03-09 16:57:54,464 [INFO] Deleting transfer
2026-03-09 16:57:54,539 [INFO] Deleted transfer=570 | completed in 0.08 sec
2026-03-09 16:57:54,539 [INFO] Deleting payment_option_metadata
2026-03-09 16:57:54,578 [INFO] Deleted payment_option_metadata=360 | completed in 0.04 sec
2026-03-09 16:57:54,578 [INFO] Deleting payment_option
2026-03-09 16:57:54,656 [INFO] Deleted payment_option=360 | completed in 0.08 sec
2026-03-09 16:57:54,656 [INFO] Deleting payment_position
2026-03-09 16:57:54,706 [INFO] Deleted payment_position=360 | completed in 0.05 sec
2026-03-09 16:57:54,751 [INFO] Cleanup committed successfully
2026-03-09 16:57:54,753 [INFO] Total execution time: 1.14 seconds
```

### Real execution example in batch mode

```text
2026-03-09 17:10:02,100 [INFO] Starting cleanup process
2026-03-09 17:10:02,101 [INFO] Parameters | service_type=WISP | payment_position_status=VALID | allow_paid=False | dry_run=False
2026-03-09 17:10:02,101 [INFO] Batch delete mode enabled | batch_size=1000
2026-03-09 17:10:02,450 [INFO] Payment positions selected: 4280
2026-03-09 17:10:02,700 [INFO] Starting batch 1 | configured_batch_size=1000 | remaining_targets_before_batch=4280
2026-03-09 17:10:03,540 [INFO] Batch 1 | deleted transfer_metadata=5000 | duration=0.84 sec
2026-03-09 17:10:03,940 [INFO] Batch 1 | deleted transfer=3200 | duration=0.40 sec
2026-03-09 17:10:04,100 [INFO] Batch 1 | deleted payment_option_metadata=1000 | duration=0.16 sec
2026-03-09 17:10:04,210 [INFO] Batch 1 | deleted payment_option=1000 | duration=0.11 sec
2026-03-09 17:10:04,300 [INFO] Batch 1 | deleted payment_position=1000 | duration=0.09 sec
2026-03-09 17:10:04,360 [INFO] Completed batch 1 | consumed_target_ids=1000 | remaining_targets_after_batch=3280 | batch_duration=1.66 sec
2026-03-09 17:10:09,880 [INFO] Completed batch 5 | consumed_target_ids=280 | remaining_targets_after_batch=0 | batch_duration=0.54 sec
2026-03-09 17:10:09,881 [INFO] Batch cleanup committed successfully
2026-03-09 17:10:09,882 [INFO] Total execution time: 7.78 seconds
```

---

## Example for the Trento WISP cleanup task

Assuming the organization fiscal codes are stored in `trento_wisp_org_codes.txt`.

### Dry-run with retention of 30 days

```bash
python delete-entities.py \
  --db-host <DB_HOST> \
  --db-port 5432 \
  --db-name apd \
  --db-user <DB_USER> \
  --db-password '<DB_PASSWORD>' \
  --org-codes-file trento_wisp_org_codes.txt \
  --service-type WISP \
  --payment-position-status VALID \
  --retention-days 30 \
  --dry-run
```

### Real execution with retention of 30 days and batch mode

```bash
python delete-entities.py \
  --db-host <DB_HOST> \
  --db-port 5432 \
  --db-name apd \
  --db-user <DB_USER> \
  --db-password '<DB_PASSWORD>' \
  --org-codes-file trento_wisp_org_codes.txt \
  --service-type WISP \
  --payment-position-status VALID \
  --retention-days 30 \
  --batch-size 1000
```

---

## Recommended execution flow

For a production cleanup, the recommended sequence is:

1. Run a **dry-run**
2. Validate:
   - selected `payment_position` count
   - dependent row counts
   - retention cutoff date
3. Run the real cleanup
4. Review `cleanup.log`

---

## Notes

- Temporary tables are automatically dropped when the database session closes.
- In batch mode, the frozen target set is still built once, but processed in smaller chunks.
- Batch mode is recommended for large cleanups to reduce transaction duration and lock pressure.
