export default [
  {
    match: {

    },
    callback: {
      url: 'http://uuid-generation/delta',
      method: 'POST'
    },
    options: {
      resourceFormat: 'v0.0.1',
      gracePeriod: 250,
      foldEffectiveChanges: false,
      ignoreFromSelf: true,
      _comment: "We explicitly don't want to fold effective changes, since uuid-generation service only handles on inserts with pattern '?s a <type-uri>'"
    }
  }
];
