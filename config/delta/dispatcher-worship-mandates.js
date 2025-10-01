export default [
  {
    match: {
      graph: {
        type: 'uri',
        value: 'http://eredienst-mandatarissen-consumer/temp-inserts'
      }
    },
    callback: {
      url: 'http://dispatcher-worship-mandates/delta-inserts',
      method: 'POST'
    },
    options: {
      resourceFormat: 'v0.0.1',
      gracePeriod: 0, //TODO: check if this should really be '0' (Ben?)
      foldEffectiveChanges: true,
      ignoreFromSelf: true,
      optOutMuScopeIds: [ "http://worship-positions-graph-dispatcher/update" ]
    }
  },
  {
    match: {
      graph: {
        type: 'uri',
        value: 'http://eredienst-mandatarissen-consumer/temp-deletes'
      }
    },
    callback: {
      url: 'http://dispatcher-worship-mandates/delta-deletes',
      method: 'POST'
    },
    options: {
      resourceFormat: 'v0.0.1',
      gracePeriod: 0, //TODO: check if this should really be '0' (Ben?)
      foldEffectiveChanges: true,
      ignoreFromSelf: true,
      optOutMuScopeIds: [ "http://worship-positions-graph-dispatcher/update" ]
    }
  },
];
