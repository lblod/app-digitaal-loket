export default [
  {
    match: {
      subject: {}
    },
    callback: {
      url: 'http://resource/.mu/delta',
      method: 'POST'
    },
    options: {
      resourceFormat: 'v0.0.1',
      gracePeriod: 250,
      foldEffectiveChanges: true,
      ignoreFromSelf: true,
      optOutMuScopeIds: [ 
        "http://redpencil.data.gift/id/concept/muScope/deltas/initialSync",
        "http://redpencil.data.gift/id/concept/muScope/deltas/vendor-data"
      ]
    }
  }
];
