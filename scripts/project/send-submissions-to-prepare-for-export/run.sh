#!/bin/bash

if [ "$#" -ne 1 ]; then
  echo "The number of parameters is not correct. You need to provide the following: ./run.sh ipContainerPrepareSubmissionsForExport"
  exit 1
fi

CONTAINER_IP=$1

while IFS="" read -r l || [ -n "$l" ]
do
  curl --location --request POST $CONTAINER_IP/delta \
  --header 'Content-Type: application/vnd.api+json' \
  --data-raw ' [
    {
      "deletes": [],
      "inserts": [
        {
          "subject": { "type": "uri", "value": "'$l'" },
          "predicate": { "type": "uri", "value": "http://mu.semte.ch/vocabularies/ext/formSubmissionStatus" },
          "object": { "type": "uri", "value": "http://lblod.data.gift/concepts/9bd8d86d-bb10-4456-a84e-91e9507c374c" }
        } 
      ]  
    }
  ]'
done < form-data-uris.txt
