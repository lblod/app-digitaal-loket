export default [
  {
    match: {
      predicate: {
        type: 'uri',
        value: 'http://www.w3.org/ns/adms#status'
      },
    },
    callback: {
      url: 'http://berichtencentrum-melding/delta',
      method: 'POST'
    },
    options: {
      resourceFormat: 'v0.0.1',
      gracePeriod: 0,
      ignoreFromSelf: true,
      optOutMuScopeIds: [
                          "http://redpencil.data.gift/id/concept/muScope/deltas/initialSync",
                          "http://redpencil.data.gift/id/concept/muScope/deltas/publicationGraphMaintenance"
                        ]
    }
  },
];
