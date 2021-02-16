/* USED PREFIXES */

let prefixes = {
  pav: 'http://purl.org/pav/',
  lblodSubsidie: 'http://lblod.data.gift/vocabularies/subsidie/',
  transactie: 'https://data.vlaanderen.be/ns/transactie#',
  schema: 'http://schema.org/',
  subsidie: 'http://data.vlaanderen.be/ns/subsidie#',
  prov: 'http://www.w3.org/ns/prov#',
  m8g: 'http://data.europa.eu/m8g/',
  terms: 'http://purl.org/dc/terms/',
};

/* MAPPINGS TO BE PROCESSED (in order) */
let mapping = [
  {
    type: 'resource',
    from: 'pav:createdBy',
    as: 'applicant',
  },
  {
    type: 'property',
    from: 'lblodSubsidie:subsidyMeasure',
    resource: 'consumption',
    predicate: 'transactie:isInstantieVan',
  },
  {
    type: 'property',
    from: 'lblodSubsidie:totalAmount',
    resource: 'requested_amount',
    predicate: 'schema:value',
  }
]

/* RESOURCE DECLARATIONS */
let resource_definitions = {};

resource_definitions['consumption'] = {
  type: 'subsidie:SubsidiemaatregelConsumptie',
  base: 'http://data.lblod.info/subsidiemaatregel-consumpties/',
  references: [
    {
      via: 'prov:wasGeneratedBy',
      as: 'request',
    },
    {
      via: 'm8g:hasParticipation',
      as: 'applicant_participation',
    },
  ],
};

resource_definitions['applicant_participation'] = {
  type: 'm8g:Participation',
  base: 'http://data.lblod.info/participaties/',
  properties: [
    {
      predicate: 'terms:description',
      object: {
        value: 'Aanvrager van de subsidie',
        datatype: 'string',
      },
    },
    {
      predicate: 'm8g:role',
      object: {
        value: 'http://lblod.data.gift/concepts/d8b8f3d1-7574-4baf-94df-188a7bd84a3a',
        datatype: 'uri',
      },
    },
  ],
};

resource_definitions['applicant'] = {
  references: [
    {
      via: 'm8g:playsRole',
      as: 'applicant_participation',
    },
  ],
};

resource_definitions['request'] = {
  type: 'subsidie:Aanvraag',
  base: 'http://data.lblod.info/aanvraagen/',
  properties: [
    {
      predicate: 'subsidie:aanvraagdatum',
      object: '$NOW',
    },
    {
      predicate: 'prov:used',
      object: '$ROOT',
    },
  ],
  references: [
    {
      via: 'prov:generated',
      as: 'consumption',
    },
    {
      via: 'subsidie:aangevraagdBedrag',
      as: 'requested_amount',
    },
  ],
};

resource_definitions['requested_amount'] = {
  type: 'schema:MonetaryAmount',
  base: 'http://data.lblod.info/aangevraagd-bedragen/',
  properties: [
    {
      predicate: 'schema:currency',
      object: {
        value: 'EUR',
        datatype: 'string',
      },
    },
  ],
};

module.exports = {
  prefixes, resource_definitions, mapping,
};