const bikeSubsidyRequestExtractor = require('./extractors/bike-subsidy-request-extractor');
const missingTypesExtractor = require('./extractors/missing-types-extractor');

/**
 * NOTE: order of execution bound to position in the array
 */
module.exports = [
    bikeSubsidyRequestExtractor, missingTypesExtractor
]