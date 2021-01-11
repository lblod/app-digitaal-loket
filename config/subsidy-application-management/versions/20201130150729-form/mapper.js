
/* MAPPINGS TO BE PROCESSED (harvested, mapped, enhanced)*/
let mapping = {};

mapping['<http://purl.org/pav/createdBy>'] = {
  type: 'resource',
  required: true,
  resource: 'applicant',
};

mapping['<http://lblod.data.gift/vocabularies/subsidie/subsidyMeasure>'] = {
  type: 'property',
  required: true,
  resource: 'consumption',
  's-prefix': 'https://data.vlaanderen.be/ns/transactie#isInstantieVan',
};

mapping['<http://lblod.data.gift/vocabularies/subsidie/totalAmount>'] = {
  type: 'property',
  required: false,
  resource: 'requested_amount',
  's-prefix': 'http://schema.org/value',
};

/* RESOURCE DECLARATIONS */
let resource_declarations = {};

resource_declarations['consumption'] = {
  type: 'http://data.vlaanderen.be/ns/subsidie#SubsidiemaatregelConsumptie',
  base: 'http://data.lblod.info/subsidiemaatregel-consumpties/',
  references: [
    {
      via: 'http://www.w3.org/ns/prov#wasGeneratedBy',
      as: 'request',
    },
    {
      via: 'http://data.europa.eu/m8g/hasParticipation',
      as: 'applicant_participation',
    },
  ],
};

resource_declarations['applicant_participation'] = {
  type: 'http://data.europa.eu/m8g/Participation',
  base: 'http://data.lblod.info/participaties/',
  properties: [
    {
      predicate: 'http://purl.org/dc/terms/description',
      object: {
        value: 'Aanvrager van de subsidie',
        datatype: 'string',
      }
    },
    {
      predicate: 'http://data.europa.eu/m8g/role',
      object: {
        value: 'http://lblod.data.gift/concepts/d8b8f3d1-7574-4baf-94df-188a7bd84a3a',
        datatype: 'uri',
      }
    },
  ],
};

resource_declarations['applicant'] = {
  references: [
    {
      via: 'http://data.europa.eu/m8g/playsRole',
      as: 'applicant_participation',
    },
  ],
};

resource_declarations['request'] = {
  type: 'http://data.vlaanderen.be/ns/subsidie#Aanvraag',
  base: 'http://data.lblod.info/aanvraagen/',
  properties: [
    {
      predicate: 'http://data.vlaanderen.be/ns/subsidie#aanvraagdatum',
      object: {
        value: '$NOW',
      }
    },
    {
      predicate: 'http://www.w3.org/ns/prov#used',
      object: {
        value: '$ROOT',
      }
    }
  ],
  references: [
    {
      via: 'http://www.w3.org/ns/prov#generated',
      as: 'consumption',
    },
    {
      via: 'http://data.vlaanderen.be/ns/subsidie#aangevraagdBedrag',
      as: 'requested_amount',
    },
  ]
};

resource_declarations['requested_amount'] = {
  type: 'http://schema.org/MonetaryAmount',
  base: 'http://data.lblod.info/aangevraagd-bedragen/',
  properties: [
    {
      predicate: 'http://schema.org/currency',
      object: {
        value: 'EUR',
        datatype: 'string'
      }
    }
  ],
}

module.exports = {
  resource_declarations, mapping
};