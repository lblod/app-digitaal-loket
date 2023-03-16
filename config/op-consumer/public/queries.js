const {
  RDF_TYPE
} = require('./constants');

const {
  INGEST_GRAPH,
} = require('./config');


const subject_type_query = (graph, subjects) => `
SELECT ?subject ?type
FROM <${graph}>
WHERE {
  VALUES ?subject {
    ${subjects.join('\n    ')}
  }
  ?subject a ?type
}`

async function findSubjectTypes(subjects, sparqlClient) {
  return await sparqlClient(subject_type_query(INGEST_GRAPH, subjects))
    .then(function (response) {
      return response.results.bindings.map(
        r => ({
          'graph': INGEST_GRAPH,
          'subject': r.subject.value,
          'predicate': RDF_TYPE,
          'object': r.type.value
        })
      )
    })
    .catch(function (err) {
      throw new Error(`Error while querying for types: ${err} `);
    });
}

module.exports = {
  findSubjectTypes
};