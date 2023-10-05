export default [
  {
    match: {
      //Send everything
    },
    callback: {
      url: 'http://vendor-data-distribution/delta',
      method: 'POST'
    },
    options: {
      resourceFormat: 'v0.0.1',
      gracePeriod: 1000,
      ignoreFromSelf: true
    }
  },
];
