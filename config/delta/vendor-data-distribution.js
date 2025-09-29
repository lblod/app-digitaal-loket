export default [
  //Submissions via Automatic Submission Flow
  //Berichten via Berichten Melding
  //Berichten via Berichten Sync Kaliope
  {
    match: {
      predicate: {
        type: 'uri',
        value: 'http://www.w3.org/ns/adms#status'
      },
    },
    callback: {
      url: 'http://vendor-data-distribution/delta',
      method: 'POST'
    },
    options: {
      resourceFormat: 'v0.0.1',
      gracePeriod: 1000,
      foldEffectiveChanges: true,
      ignoreFromSelf: true
    }
  },
  //Berichten from Loket
  {
    match: {
      predicate: {
        value: 'http://mu.semte.ch/vocabularies/ext/creator',
      },
      object: {
        value: 'https://github.com/lblod/frontend-loket',
      },
    },
    callback: {
      url: 'http://vendor-data-distribution/delta',
      method: 'POST'
    },
    options: {
      resourceFormat: 'v0.0.1',
      gracePeriod: 1000,
      foldEffectiveChanges: true,
      ignoreFromSelf: true
    }
  },
];
