# Summary

This scripts gets the ip of the `prepare-submissions-for-export-container` and submits to it form data URI to be re-processed as deltas.

# Requirements

You should get all the form data URIs that you want to re-process and put them in a file named `form-data-uris.txt` in this folder. Please remove this file after the execution of the script to avoid cluttering the repo.

# Use case: re-process submissions with multiple types

## Get the form data URIs

Run the following query to get the form data URIs
```
select distinct ?formData where {
  ?submission a <http://rdf.myexperiment.org/ontologies/base/Submission> ;
    <http://www.w3.org/ns/prov#generated> ?formData ;
    <http://purl.org/dc/terms/created> ?created .

  ?formData <http://mu.semte.ch/vocabularies/ext/formSubmissionStatus> <http://lblod.data.gift/concepts/9bd8d86d-bb10-4456-a84e-91e9507c374c> ;
    <http://mu.semte.ch/vocabularies/ext/decisionType> ?firstDecisionType ;
    <http://mu.semte.ch/vocabularies/ext/decisionType> ?secondDecisionType .

  ?firstDecisionType skos:prefLabel ?firstDecisionTypeLabel .
  ?secondDecisionType skos:prefLabel ?secondDecisionTypeLabel .

  FILTER (?firstDecisionType > ?secondDecisionType)

  FILTER NOT EXISTS {
    ?submission <http://schema.org/publication> ?flag.
  }
}
```

## Follow up of flagged submissions

Run the following query to get number of unflagged submissions with multiple types
```
select distinct ?submission ?created ?firstDecisionTypeLabel ?secondDecisionTypeLabel where {
  ?submission a <http://rdf.myexperiment.org/ontologies/base/Submission> ;
    <http://www.w3.org/ns/prov#generated> ?formData ;
    <http://purl.org/dc/terms/created> ?created .

  ?formData <http://mu.semte.ch/vocabularies/ext/formSubmissionStatus> <http://lblod.data.gift/concepts/9bd8d86d-bb10-4456-a84e-91e9507c374c> ;
    <http://mu.semte.ch/vocabularies/ext/decisionType> ?firstDecisionType ;
    <http://mu.semte.ch/vocabularies/ext/decisionType> ?secondDecisionType .

  ?firstDecisionType skos:prefLabel ?firstDecisionTypeLabel .
  ?secondDecisionType skos:prefLabel ?secondDecisionTypeLabel .

  FILTER (?firstDecisionType > ?secondDecisionType)

  FILTER NOT EXISTS {
    ?submission <http://schema.org/publication> ?flag.
  }
}
order by ?submission
```