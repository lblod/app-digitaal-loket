const bikeSubsidyRequestExtractor = require('./extractors/bike-subsidy-request-extractor');

/**
 * NOTE: order of execution bound to position in the array
 */
module.exports = [
    bikeSubsidyRequestExtractor
]