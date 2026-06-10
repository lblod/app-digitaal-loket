# restore-virtuoso-backup

Restore the Virtuoso triplestore from an online `.bp` backup in `data/db/backups/`
(created by `mu script virtuoso create-backup`). The image ships `create-backup` but no
restore of its own.

**Destructive** — replaces the current database in `data/db`. Stop the `virtuoso` service
first.

## Usage

From the project root:

```bash
docker compose stop virtuoso

mu script project-scripts restore-backup                  # auto-detect a single backup set
mu script project-scripts restore-backup <prefix> --yes   # pin a set / skip the prompt

docker compose up -d virtuoso
```

Without mu-cli, run the container directly:

```bash
docker run --rm -it -v "$PWD":/project redpencil/virtuoso:1.2.2 \
  bash /project/scripts/restore-virtuoso-backup/run.sh [<prefix>] [--yes]
```

## Parameters

| Arg | Meaning |
|-----|---------|
| `prefix` | Backup prefix up to the numeric suffix (e.g. `backup_20260609_120000_`). Auto-detected when `data/db/backups/` holds one set; required if it holds several. |
| `--yes` | Skip the confirmation prompt. |

Pinned to `redpencil/virtuoso:1.2.2` to match the `virtuoso` service — bump both together.
