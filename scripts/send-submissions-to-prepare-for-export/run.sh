#!/bin/bash

while IFS="" read -r l || [ -n "$l" ]
do
  curl --location --request POST "$(docker inspect "$(docker compose ps -q prepare-submissions-for-export)" --format '{{range .NetworkSettings.Networks}}{{.IPAddress}}{{end}}')"/delta \
  --header 'Content-Type: application/vnd.api+json' \
  --data-raw ' [
    {
      "deletes": [],
      "inserts": [
        {
          "subject": { "type": "uri", "value": "'"$l"'" },
          "predicate": { "type": "uri", "value": "http://mu.semte.ch/vocabularies/ext/formSubmissionStatus" },
          "object": { "type": "uri", "value": "http://lblod.data.gift/concepts/9bd8d86d-bb10-4456-a84e-91e9507c374c" }
        }
      ]
    }
  ]'
done < form-data-uris.txt
