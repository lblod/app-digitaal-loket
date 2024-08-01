# Sync mandatenbeheer entities (POC)

Lokaal Mandatenbeheer ([LMB](https://github.com/lblod/app-lokaal-mandatenbeheer)) will manage all madatarissen. These mandatarissen are build up from different entities than can be edited. (Fractie,..) To keep Loket in sync with all these changes we want to consume the LDES data from the LMB LDES feed.

## Services

- LDES consumer (redpencil/ldes-consumer:0.8.0-rc1)
- Upsert entities (lblod/ldes-make-object-service:0.1.1)
- Deltanotifier (cecemel/delta-notifier:0.2.0-beta.2)

### LDES consumer

With the current configuration in the compose file we are consuming the ldes stream (ldes-backend) that is in the same docker network as our consumer.

The consumer will find versioned triples on the feed and is writing them to our staging graph as we do not want the versioned triples to be in the database of loket. To Insert or update the triples that we get from the ldes-feed we use the `ldes-make-object-service`.

### Ldes-make-object-service

This service will look what the delta-notifier is producing on POST `/delta`. To make sure that only the triples from our staging graph (`LANDING_ZONE_GRAPH`) are being passed on this method we wrote a delta-rule.

When the triple is passed on the `/post` method that versioned triple will be transformed to the `isVersionOf` to create or update the actual entity. This entity is than upserted to the loket database (`TARGET_GRAPH`).

### Deltanotifier

No extra confiruation for this service was done only writing the rule to make sure that only the triples from our staging graph (`LANDING_ZONE_GRAPH`) are being processed.
