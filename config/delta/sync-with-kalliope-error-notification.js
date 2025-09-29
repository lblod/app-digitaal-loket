export default [
  {
    match: {
      predicate: {
        type: 'uri',
        value: 'http://purl.org/pav/createdBy'
      },
      object: {
        type: 'uri',
        value: 'http://lblod.data.gift/services/berichtencentrum-sync-with-kalliope-service'
      }
    },
    callback: {
      url: 'http://sync-with-kalliope-error-notification/delta',
      method: 'POST'
    },
    options: {
      resourceFormat: 'v0.0.1',
      gracePeriod: 1000,
      foldEffectiveChanges: true,
      ignoreFromSelf: true,
      optOutMuScopeIds: [
                          "http://redpencil.data.gift/id/concept/muScope/deltas/initialSync",
                          "http://redpencil.data.gift/id/concept/muScope/deltas/publicationGraphMaintenance"
                        ]
    }
  },
];
