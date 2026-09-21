# restore-virtuoso-backup

Restore the Virtuoso triplestore from an online `.bp` backup in `data/db/backups/`

:warning: REPLACES the current database in `data/db`. :warning:

Stop the `virtuoso` service first.

## Usage

From the project root:

```bash
docker compose stop virtuoso

mu script project-scripts restore-backup
mu script project-scripts restore-backup <prefix>

docker compose up -d virtuoso
```

## Parameters

| Arg | Meaning |
|-----|---------|
| `prefix` | Backup prefix up to the numeric suffix (e.g. `backup_20260609_120000_`). Auto-detected when `data/db/backups/` holds one set; required if it holds several. |

Pinned to `redpencil/virtuoso:1.2.2` to match the `virtuoso` service — bump both together.
