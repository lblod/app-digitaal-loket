
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
  resource: 'request',
  's-prefix': 'http://data.vlaanderen.be/ns/subsidie#aangevraagdBedrag',
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
        value: 'applicant who requested the subsidy',
        datatype: 'string',
      }
    },
    {
      predicate: 'http://data.europa.eu/m8g/role',
      object: {
        value: 'http://whatever-the-applicant-role-will-be', // TODO add the correct role
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
  ]
};

module.exports = {
  resource_declarations, mapping
};