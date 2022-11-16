export default [
 {
    match: {
      predicate: {
        type: 'uri',
        value: 'http://purl.org/dc/terms/isVersionOf'
      }
    },
    callback: {
      url: 'http://lpdc-management/delta',
      method:'POST'
    },
    options: {
      resourceFormat: 'v0.0.1',
      gracePeriod: 1000,
      ignoreFromSelf: true,
      optOutMuScopeIds: [
        "http://redpencil.data.gift/id/concept/muScope/deltas/initialSync",
        "http://redpencil.data.gift/id/concept/muScope/deltas/publicationGraphMaintenance"
      ]
    }
  }
];
