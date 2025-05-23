export default [
  {
    match: {
      predicate: {
        type: 'uri',
        value: 'http://redpencil.data.gift/vocabularies/tasks/operation'
      },
      object: {
        type: 'uri',
        value: 'http://lblod.data.gift/id/jobs/concept/TaskOperation/import'
      }
    },
    callback: {
      url: 'http://import-submission/delta',
      method: 'POST'
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
