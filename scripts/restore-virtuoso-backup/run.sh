#!/bin/bash
# Restore the Virtuoso triplestore from an online backup (the .bp files in
# data/db/backups, as produced by `mu script virtuoso create-backup`).
#
# Runs offline in a one-off redpencil/virtuoso container (same image as the
# `virtuoso` service) with the project mounted at /project. The live `virtuoso`
# service MUST be stopped first:   docker compose stop virtuoso
#
# DESTRUCTIVE: deletes the current database in data/db and rebuilds it from the
# backup. See ./README.md for usage.
set -euo pipefail

PROJECT="${PROJECT_DIR:-/project}"
DATADIR="$PROJECT/data/db"
BACKUPS="$DATADIR/backups"
INI="${VIRTUOSO_INI:-$PROJECT/config/virtuoso/virtuoso.ini}"

# --- args: optional [prefix] and optional --yes (skip confirmation) ----------
PREFIX=""
FORCE=0
for arg in "$@"; do
  case "$arg" in
    --yes|-y) FORCE=1 ;;
    *)        PREFIX="$arg" ;;
  esac
done

# --- sanity checks -----------------------------------------------------------
[ -d "$DATADIR" ] || { echo "ERROR: $DATADIR not found (run from the project root)."; exit 1; }
[ -f "$INI" ]     || { echo "ERROR: virtuoso.ini not found at $INI."; exit 1; }
[ -d "$BACKUPS" ] || { echo "ERROR: no backups folder at $BACKUPS."; exit 1; }

if [ -e "$DATADIR/virtuoso.lck" ]; then
  echo "ERROR: $DATADIR/virtuoso.lck is present — the database looks like it is still running."
  echo "       Stop it first:   docker compose stop virtuoso"
  echo "       (If you are certain it is stopped, remove the stale lock file and retry.)"
  exit 1
fi

# --- resolve the backup prefix ----------------------------------------------
shopt -s nullglob
bpfiles=("$BACKUPS"/*.bp)
shopt -u nullglob
[ "${#bpfiles[@]}" -gt 0 ] || { echo "ERROR: no .bp backup files found in $BACKUPS."; exit 1; }

if [ -z "$PREFIX" ]; then
  # Derive the prefix = filename up to the trailing numeric suffix (e.g.
  # backup_20260609_120000_3.bp -> backup_20260609_120000_).
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

# --- confirm (destructive) ---------------------------------------------------
if [ "$FORCE" -ne 1 ]; then
  if ! read -r -p "This DELETES the current database and restores '$PREFIX'. Type 'yes' to continue: " CONFIRM; then
    echo "Aborted (no input — pass --yes to skip this prompt)."; exit 1
  fi
  [ "$CONFIRM" = "yes" ] || { echo "Aborted."; exit 1; }
fi

# --- remove the current database (Method 1 prerequisite) ---------------------
echo "Removing current database files..."
rm -f "$DATADIR"/virtuoso.db \
      "$DATADIR"/virtuoso.trx \
      "$DATADIR"/virtuoso.pxa \
      "$DATADIR"/virtuoso-temp.db \
      "$DATADIR"/.dba_pwd_set \
      "$DATADIR"/.backup_restored

# --- restore -----------------------------------------------------------------
# The live virtuoso.ini points the database files at absolute paths inside the
# image (/usr/local/.../db, symlinked to /data). In this one-off container the
# host data dir is at $DATADIR instead, so rewrite the paths onto it — the
# rebuilt database then lands in ./data/db on the host, where the real service
# will find it on next start.
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

# --- stop toLoad/ from re-seeding the restored database ----------------------
# On startup the entrypoint imports data/db/toLoad/ when .data_loaded is absent.
# A restored backup is already complete, so guard against a duplicate import.
if [ -d "$DATADIR/toLoad" ] && [ -n "$(ls -A "$DATADIR/toLoad" 2>/dev/null)" ]; then
  touch "$DATADIR/.data_loaded"
  echo "NOTE: created data/db/.data_loaded so the toLoad/ seed is NOT re-imported on top of"
  echo "      the restored database. Delete it if you DO want toLoad re-loaded on next start."
fi

echo
echo "Restore complete. Start the database again:"
echo "   docker compose up -d virtuoso"
