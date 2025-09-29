export default [
  {
    match: {
      predicate: {
        type: 'uri',
        value: 'http://redpencil.data.gift/vocabularies/tasks/operation'
      },
      object: {
        type: 'uri',
        value: 'http://lblod.data.gift/id/jobs/concept/TaskOperation/form-data-generate'
      }
    },
    callback: {
      url: 'http://toezicht-flattened-form-data-generator/automatic/delta',
      method: 'POST'
    },
    options: {
      resourceFormat: 'v0.0.1',
      gracePeriod: 1000,
      foldEffectiveChanges: true,
      ignoreFromSelf: true,
      optOutMuScopeIds: [
                          "http://redpencil.data.gift/id/concept/muScope/deltas/initialSync",
                          "http://redpencil.data.gift/id/concept/muScope/deltas/publicationGraphMaintenance",
                          "http://redpencil.data.gift/id/concept/muScope/deltas/vendor-data"
      ]
    }
  },
  {
    match: {
      predicate: {
        type: 'uri',
        value: 'http://www.w3.org/ns/adms#status'
      },
      object: {
        type: 'uri',
        value: 'http://lblod.data.gift/concepts/9bd8d86d-bb10-4456-a84e-91e9507c374c'
      }
    },
    callback: {
      url: 'http://toezicht-flattened-form-data-generator/manual/delta',
      method: 'POST'
    },
    options: {
      resourceFormat: 'v0.0.1',
      gracePeriod: 1000,
      foldEffectiveChanges: true,
      ignoreFromSelf: true,
      optOutMuScopeIds: [
                          "http://redpencil.data.gift/id/concept/muScope/deltas/initialSync",
                          "http://redpencil.data.gift/id/concept/muScope/deltas/publicationGraphMaintenance",
                          "http://redpencil.data.gift/id/concept/muScope/deltas/vendor-data"
      ]
    }
  },
];
