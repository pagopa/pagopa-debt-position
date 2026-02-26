# delete-entities.py

Cleanup utility for the APD database.

This script is designed to delete `payment_position` records and all dependent rows in the correct foreign-key order, using **set-based SQL operations**

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
- Reusable for future cleanup tasks

---

## Functional selection criteria

By default, the script targets `payment_position` rows matching:

- `organization_fiscal_code` in the provided list
- `service_type = 'WISP'` (unless overridden)
- `status = 'VALID'` (unless overridden)
- `payment_date IS NULL`

If you explicitly pass `--allow-paid`, the filter on `payment_date IS NULL` is removed.

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

### Execution mode parameters

- `--dry-run` : simulate the cleanup without deleting data

---

## How dry-run works

Dry-run mode is the safest way to validate the cleanup before running the real deletion.

When `--dry-run` is used, the script:

1. connects to the database
2. creates temporary tables
3. loads the organization codes
4. freezes the target `payment_position` IDs
5. counts how many target rows and dependent rows are involved
6. writes process logs
7. performs a **rollback instead of commit**

With dry-run mode:

- **no data is deleted**
- you can validate the exact scope of the cleanup
- you can estimate runtime and impact before the real execution

---

## Logging

The script logs:

- start of execution
- number of organization codes loaded
- number of target `payment_position` rows
- counts of dependent rows
- start/end of each delete phase
- execution time per phase
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

---

## Example output

Dry run execution example:

```text
2026-03-09 16:52:28,585 [INFO] Starting cleanup process
2026-03-09 16:52:28,995 [INFO] Creating temporary tables
2026-03-09 16:52:29,108 [INFO] Loaded 1 organization codes
2026-03-09 16:52:29,144 [INFO] Freezing payment_position target set
2026-03-09 16:52:29,214 [INFO] Payment positions selected: 360
2026-03-09 16:52:29,214 [INFO] Counting dependent records
2026-03-09 16:52:29,317 [INFO] payment_position: 360
2026-03-09 16:52:29,318 [INFO] payment_option: 360
2026-03-09 16:52:29,318 [INFO] transfer: 570
2026-03-09 16:52:29,318 [INFO] Dry-run enabled: rollback will be executed
```

Real execution example:

```text
2026-03-09 16:57:53,614 [INFO] Starting cleanup process
2026-03-09 16:57:54,041 [INFO] Creating temporary tables
2026-03-09 14:57:54,165 [INFO] Loaded 1 organization codes
2026-03-09 16:57:54,211 [INFO] Freezing payment_position target set
2026-03-09 16:57:54,300 [INFO] Payment positions selected: 360
2026-03-09 16:57:54,301 [INFO] Counting dependent records
2026-03-09 16:57:54,418 [INFO] payment_position: 360
2026-03-09 16:57:54,419 [INFO] payment_option: 360
2026-03-09 16:57:54,419 [INFO] transfer: 570
2026-03-09 16:57:54,420 [INFO] Deleting transfer_metadata
2026-03-09 16:57:54,463 [INFO] Completed in 0.04 sec
2026-03-09 16:57:54,464 [INFO] Deleting transfer
2026-03-09 16:57:54,539 [INFO] Completed in 0.08 sec
2026-03-09 16:57:54,539 [INFO] Deleting payment_option_metadata
2026-03-09 16:57:54,578 [INFO] Completed in 0.04 sec
2026-03-09 16:57:54,578 [INFO] Deleting payment_option
2026-03-09 16:57:54,656 [INFO] Completed in 0.08 sec
2026-03-09 16:57:54,656 [INFO] Deleting payment_position
2026-03-09 16:57:54,706 [INFO] Completed in 0.05 sec
2026-03-09 16:57:54,751 [INFO] Cleanup committed successfully
2026-03-09 16:57:54,753 [INFO] Total execution time: 1.14 seconds
```

---


