#!/bin/bash
# Restore Virtuoso from a .bp backup in data/db/backups. Destructive: replaces
# the current database. Stop the virtuoso service first. See ./README.md.
set -euo pipefail

PROJECT="${PROJECT_DIR:-/project}"
DATADIR="$PROJECT/data/db"
BACKUPS="$DATADIR/backups"
INI="${VIRTUOSO_INI:-$PROJECT/config/virtuoso/virtuoso.ini}"

# args: optional prefix
PREFIX="${1:-}"

[ -d "$DATADIR" ] || { echo "ERROR: $DATADIR not found (run from the project root)."; exit 1; }
[ -f "$INI" ]     || { echo "ERROR: virtuoso.ini not found at $INI."; exit 1; }
[ -d "$BACKUPS" ] || { echo "ERROR: no backups folder at $BACKUPS."; exit 1; }

if [ -e "$DATADIR/virtuoso.lck" ]; then
  echo "ERROR: $DATADIR/virtuoso.lck is present — the database looks like it is still running."
  echo "       Stop it first:   docker compose stop virtuoso"
  echo "       (If you are certain it is stopped, remove the stale lock file and retry.)"
  exit 1
fi

# resolve the backup prefix
shopt -s nullglob
bpfiles=("$BACKUPS"/*.bp)
shopt -u nullglob
[ "${#bpfiles[@]}" -gt 0 ] || { echo "ERROR: no .bp backup files found in $BACKUPS."; exit 1; }

if [ -z "$PREFIX" ]; then
  # prefix = filename up to the trailing numeric suffix (backup_..._3.bp -> backup_..._)
  declare -A seen=()
  for f in "${bpfiles[@]}"; do
    base="${f##*/}"
    seen["$(printf '%s' "$base" | sed -E 's/[0-9]+\.bp$//')"]=1
  done
  prefixes=("${!seen[@]}")
  if [ "${#prefixes[@]}" -gt 1 ]; then
    echo "ERROR: multiple backup sets found — pass one explicitly as the prefix:"
    printf '   %s\n' "${prefixes[@]}"
    exit 1
  fi
  PREFIX="${prefixes[0]}"
fi

shopt -s nullglob
match=("$BACKUPS/$PREFIX"*.bp)
shopt -u nullglob
[ "${#match[@]}" -gt 0 ] || { echo "ERROR: no files matching ${PREFIX}*.bp in $BACKUPS."; exit 1; }

echo "Backup set : $PREFIX  (${#match[@]} .bp files)"
echo "Target     : $DATADIR"

if ! read -r -p "This DELETES the current database and restores '$PREFIX'. Type 'yes' to continue: " CONFIRM; then
  echo "Aborted (no input)."; exit 1
fi
[ "$CONFIRM" = "yes" ] || { echo "Aborted."; exit 1; }

echo "Removing current database files..."
rm -f "$DATADIR"/virtuoso.db \
      "$DATADIR"/virtuoso.trx \
      "$DATADIR"/virtuoso.pxa \
      "$DATADIR"/virtuoso-temp.db \
      "$DATADIR"/.dba_pwd_set \
      "$DATADIR"/.backup_restored

# Rewrite the ini's DB paths onto $DATADIR so the rebuilt files land in ./data/db
# (the live ini points them at absolute in-image paths).
RESTORE_INI=/tmp/restore.ini
cp "$INI" "$RESTORE_INI"
crudini --set "$RESTORE_INI" Database     DatabaseFile       "$DATADIR/virtuoso.db"
crudini --set "$RESTORE_INI" Database     ErrorLogFile       "$DATADIR/virtuoso.log"
crudini --set "$RESTORE_INI" Database     LockFile           "$DATADIR/virtuoso.lck"
crudini --set "$RESTORE_INI" Database     TransactionFile    "$DATADIR/virtuoso.trx"
crudini --set "$RESTORE_INI" Database     xa_persistent_file "$DATADIR/virtuoso.pxa"
crudini --set "$RESTORE_INI" TempDatabase DatabaseFile       "$DATADIR/virtuoso-temp.db"
crudini --set "$RESTORE_INI" TempDatabase TransactionFile    "$DATADIR/virtuoso-temp.trx"

echo "Restoring..."
cd "$BACKUPS"
virtuoso-t +restore-backup "$PREFIX" +configfile "$RESTORE_INI"

# Entrypoint re-imports toLoad/ when .data_loaded is absent; the restore is
# already complete, so set the marker to skip it.
if [ -d "$DATADIR/toLoad" ] && [ -n "$(ls -A "$DATADIR/toLoad" 2>/dev/null)" ]; then
  touch "$DATADIR/.data_loaded"
  echo "NOTE: created data/db/.data_loaded so the toLoad/ seed is NOT re-imported on top of"
  echo "      the restored database. Delete it if you DO want toLoad re-loaded on next start."
fi

echo
echo "Restore complete. Start the database again:"
echo "   docker compose up -d virtuoso"
