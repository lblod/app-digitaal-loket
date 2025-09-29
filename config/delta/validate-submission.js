export default [
  {
    match: {
      predicate: {
        type: 'uri',
        value: 'http://redpencil.data.gift/vocabularies/tasks/operation'
      },
      object: {
        type: 'uri',
        value: 'http://lblod.data.gift/id/jobs/concept/TaskOperation/validate'
      }
    },
    callback: {
      url: 'http://validate-submission/delta',
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
