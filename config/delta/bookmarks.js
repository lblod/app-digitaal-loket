export default [
  // Get notified about new logins to prepopulate bookmarks
  {
    match: {
      predicate: {
        value: 'http://mu.semte.ch/vocabularies/ext/sessionRole'
      }
    },
    callback: {
      url: 'http://ipdc-bookmarks/delta',
      method: 'POST'
    },
    options: {
      resourceFormat: 'v0.0.1',
      gracePeriod: 250,
      foldEffectiveChanges: true,
      ignoreFromSelf: true,
    }
  }
]
