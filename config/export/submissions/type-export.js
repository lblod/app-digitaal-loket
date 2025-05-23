import envvar from 'env-var';
import { sparqlEscapeString } from 'mu';

const NUM_OF_DAYS = envvar
  .get('NUM_OF_DAYS')
  .required()
  .default('1')
  .asIntPositive();

export const constructDateFilter = () => {
  const today = new Date();

  // Set the upper bound of the date comparison.
  // Example:
  // If today is 2024-10-16, this will set tomorrow to
  // 2024-10-17T00:00:00.000Z
  const tomorrow = new Date(today);
  tomorrow.setUTCDate(tomorrow.getUTCDate() + 1);
  tomorrow.setUTCHours(0, 0, 0, 0);

  // Set the lower bound of the date comparison.
  // Example:
  // If today is 2024-10-16 and NUM_OF_DAYS is 2, this will set cutOffDate
  // to 2024-10-14T00:00:00.000Z
  const cutOffDate = new Date(today);
  cutOffDate.setUTCDate(cutOffDate.getUTCDate() - NUM_OF_DAYS);
  cutOffDate.setUTCHours(0, 0, 0, 0);

  return `
    BIND(xsd:dateTime(${sparqlEscapeString(tomorrow.toISOString())}) AS ?tomorrow)
    BIND(xsd:dateTime(${sparqlEscapeString(cutOffDate.toISOString())}) AS ?cutOffDate)

    FILTER (?sentDate >= ?cutOffDate AND ?sentDate <= ?tomorrow) .
  `;
}

const ADVANCED_SUBMISSION_FILTER = `
?submission
  adms:status <http://lblod.data.gift/concepts/9bd8d86d-bb10-4456-a84e-91e9507c374c> ;
  prov:generated ?formData .

OPTIONAL {
  ?formData eli:passed_by/mandaat:isTijdspecialisatieVan ?orgaan .
  ?orgaan besluit:classificatie ?orgaanClass .
}

FILTER NOT EXISTS { ?formData dct:type <https://data.vlaanderen.be/id/concept/BesluitDocumentType/bea3944f-4f6d-4d2c-9a6e-23264859e1e5> . }
FILTER NOT EXISTS { ?formData dct:type <https://data.vlaanderen.be/id/concept/BesluitDocumentType/1d14cb62-7e57-44a9-ad20-2b08407fbb84> . }
FILTER NOT EXISTS { ?formData dct:type <https://data.vlaanderen.be/id/concept/BesluitDocumentType/91b8b15f-7631-4a21-9a90-489f5c91e73c> . }
FILTER NOT EXISTS { ?formData dct:type <https://data.vlaanderen.be/id/concept/BesluitDocumentType/3a3ea43f-6631-4a7d-94c6-3a77a445d450> . }
FILTER NOT EXISTS { ?formData dct:type <https://data.vlaanderen.be/id/concept/BesluitDocumentType/24743b26-e0fb-4c14-8c82-5cd271289b0e> . }
FILTER NOT EXISTS { ?formData dct:type <https://data.vlaanderen.be/id/concept/BesluitDocumentType/863caf68-97c9-4ee0-adb5-620577ea8146> . }
FILTER NOT EXISTS { ?formData dct:type <https://data.vlaanderen.be/id/concept/BesluitDocumentType/14793940-5b9c-4172-b108-c73665ad9d6a> . }
FILTER NOT EXISTS { ?formData dct:type <https://data.vlaanderen.be/id/concept/BesluitDocumentType/651525f8-8650-4ce8-8eea-f19b94d50b73> . }
FILTER NOT EXISTS { ?formData dct:type <https://data.vlaanderen.be/id/concept/BesluitDocumentType/ce569d3d-25ff-4ce9-a194-e77113597e29> . }
FILTER NOT EXISTS { ?formData dct:type <https://data.vlaanderen.be/id/concept/BesluitDocumentType/18833df2-8c9e-4edd-87fd-b5c252337349> . }
FILTER NOT EXISTS { ?formData dct:type <https://data.vlaanderen.be/id/concept/BesluitDocumentType/2c9ada23-1229-4c7e-a53e-acddc9014e4e> . }
FILTER NOT EXISTS { ?formData dct:type <https://data.vlaanderen.be/id/concept/BesluitType/d85218e2-a75f-4a30-9182-512b5c9dd1b2> . }
FILTER NOT EXISTS { ?formData dct:type <https://data.vlaanderen.be/id/concept/BesluitType/d463b6d1-c207-4c1a-8c08-f2c7dd1fa53b> . }
FILTER NOT EXISTS { ?formData dct:type <https://data.vlaanderen.be/id/concept/BesluitType/2b12630f-8c4e-40a4-8a61-a0c45621a1e6> . }
FILTER NOT EXISTS { ?formData dct:type <https://data.vlaanderen.be/id/concept/BesluitType/0fc2c27d-a03c-4e3f-9db1-f10f026f76f8> . }
FILTER NOT EXISTS { ?formData dct:type <https://data.vlaanderen.be/id/concept/BesluitType/df261490-cc74-4f80-b783-41c35e720b46> . }
FILTER NOT EXISTS { ?formData dct:type <https://data.vlaanderen.be/id/concept/BesluitType/3fcf7dba-2e5b-4955-a489-6dd8285c013b> . }
FILTER NOT EXISTS { ?formData dct:type <https://data.vlaanderen.be/id/concept/BesluitType/95c671c2-3ab7-43e2-a90d-9b096c84bfe7> . }

?formData dct:type ?typeBesluit .

BIND(
  IF((?typeBesluit = <https://data.vlaanderen.be/id/concept/BesluitDocumentType/3fa67785-ffdc-4b30-8880-2b99d97b4dee> &&
     bound(?orgaan) &&
     ?orgaanClass IN (
       <http://data.vlaanderen.be/id/concept/BestuursorgaanClassificatieCode/5733254e-73ff-4844-8d43-7afb7ec726e8>,
       <http://data.vlaanderen.be/id/concept/BestuursorgaanClassificatieCode/0dbc70ec-6be9-4997-b8e1-11b6c0542382>,
       <http://data.vlaanderen.be/id/concept/BestuursorgaanClassificatieCode/17e76b36-64a1-4db1-8927-def3064b4bf1>,
       <http://data.vlaanderen.be/id/concept/BestuursorgaanClassificatieCode/5ab0e9b8a3b2ca7c5e000009>,
       <http://data.vlaanderen.be/id/concept/BestuursorgaanClassificatieCode/53c0d8cd-f3a2-411d-bece-4bd83ae2bbc9>,
       <http://data.vlaanderen.be/id/concept/BestuursorgaanClassificatieCode/9314533e-891f-4d84-a492-0338af104065>,
       <http://data.vlaanderen.be/id/concept/BestuursorgaanClassificatieCode/5ab0e9b8a3b2ca7c5e00000b>,
       <http://data.vlaanderen.be/id/concept/BestuursorgaanClassificatieCode/180a2fba-6ca9-4766-9b94-82006bb9c709>,
       <http://data.vlaanderen.be/id/concept/BestuursorgaanClassificatieCode/e14fe683-e061-44a2-b7c8-e10cab4e6ed9>,
       <http://data.vlaanderen.be/id/concept/BestuursorgaanClassificatieCode/5ab0e9b8a3b2ca7c5e000006>,
       <http://data.vlaanderen.be/id/concept/BestuursorgaanClassificatieCode/4c38734d-2cc1-4d33-b792-0bd493ae9fc2>,
       <http://data.vlaanderen.be/id/concept/BestuursorgaanClassificatieCode/5ab0e9b8a3b2ca7c5e00000d>,
       <http://data.vlaanderen.be/id/concept/BestuursorgaanClassificatieCode/4955bd72cd0e4eb895fdbfab08da0284>,
       <http://data.vlaanderen.be/id/concept/BestuursorgaanClassificatieCode/5ab0e9b8a3b2ca7c5e000008>)),
    false, true) as ?allowedExport) .

FILTER (?allowedExport = true) .
?submission nmo:sentDate ?sentDate .
`;

export default {
  prefixes: {
    mandaat: "http://data.vlaanderen.be/ns/mandaat#",
    besluit: "http://data.vlaanderen.be/ns/besluit#",
    persoon: "http://data.vlaanderen.be/ns/persoon#",
    melding: "http://lblod.data.gift/vocabularies/automatische-melding/",
    lblodBesluit: "http://lblod.data.gift/vocabularies/besluit/",
    foaf: "http://xmlns.com/foaf/0.1/",
    org: "http://www.w3.org/ns/org#",
    skos: "http://www.w3.org/2004/02/skos/core#",
    rdf: "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
    rdfs: "http://www.w3.org/2000/01/rdf-schema#",
    ext: "http://mu.semte.ch/vocabularies/ext/",
    regorg: "https://www.w3.org/ns/regorg#",
    person: "http://www.w3.org/ns/person#",
    schema: "http://schema.org/",
    prov: "http://www.w3.org/ns/prov#",
    adms: "http://www.w3.org/ns/adms#",
    mu: "http://mu.semte.ch/vocabularies/core/",
    pav: "http://purl.org/pav/",
    dct: "http://purl.org/dc/terms/",
    nmo: "http://www.semanticdesktop.org/ontologies/2007/03/22/nmo#",
    nfo: "http://www.semanticdesktop.org/ontologies/2007/03/22/nfo#",
    nie: "http://www.semanticdesktop.org/ontologies/2007/01/19/nie#",
    dbpedia: "http://dbpedia.org/ontology/",
    eli: "http://data.europa.eu/eli/ontology#",
    meb: "http://rdf.myexperiment.org/ontologies/base/",
    elod: "http://linkedeconomy.org/ontology#",
    toezicht: "http://mu.semte.ch/vocabularies/ext/supervision/",
    ere: "http://data.lblod.info/vocabularies/erediensten/",
  },
  types: [
    {
      type: "foaf:Person",
      requiredProperties: [
        "mu:uuid"
      ],
      optionalProperties: [
        "foaf:firstName",
        "foaf:familyName"
      ],
      additionalFilter: "?s a meb:Submission ; ext:lastModifiedBy ?resource ."
    },
    {
      type: "toezicht:InzendingVoorToezicht",
      requiredProperties: [
        "mu:uuid"
      ],
      optionalProperties: [

      ],
      additionalFilter: `
?submission
  dct:source ?resource ;
  a meb:Submission ;
  nmo:sentDate ?sentDate .
      `,
      hasDateFilter: true,
    },
    {
      type: "meb:Submission",
      requiredProperties: [
        "mu:uuid"
      ],
      optionalProperties: [
        "prov:atLocation",
        "dct:created",
        "dct:modified",
        "nmo:sentDate",
        "nmo:receivedDate",
        "pav:createdBy",
        "pav:providedBy",
        "dct:subject",
        "dct:source"
      ],
      additionalFilter: `
BIND (?resource as ?submission)
${ADVANCED_SUBMISSION_FILTER}
      `,
      hasDateFilter: true,
    },
    {
      type: "meb:Submission",
      requiredProperties: [
        "mu:uuid"
      ],
      optionalProperties: [
        "adms:status",
        "prov:generated",
        "ext:lastModifiedBy",
        "dct:hasPart"
      ],
      additionalFilter: `
BIND (?resource as ?submission)
${ADVANCED_SUBMISSION_FILTER}
      `,
      hasDateFilter: true,
    },
    {
      type: "ext:SubmissionDocument",
      requiredProperties: [
        "mu:uuid"
      ],
      optionalProperties: [
        "dct:source",
        "eli:has_part"
      ],
      additionalFilter: `
?submission dct:subject ?resource .
${ADVANCED_SUBMISSION_FILTER}
      `,
      hasDateFilter: true,
    },
    {
      type: "ext:SubmissionDocumentStatus",
      requiredProperties: [
        "mu:uuid"
      ],
      optionalProperties: [
        "skos:prefLabel"
      ],
      additionalFilter: ""
    },
    {
      type: "besluit:Artikel",
      requiredProperties: [
      ],
      optionalProperties: [
        "mu:uuid",
        "schema:publication",
        "eli:type_document",
        "eli:refers_to"
      ],
      additionalFilter: ""
    },
    {
      type: "lblodBesluit:TaxRate",
      requiredProperties: [
        "mu:uuid"
      ],
      optionalProperties: [
        "schema:price"
      ],
      additionalFilter: ""
    },
    {
      type: "ext:AuthenticityType",
      requiredProperties: [
        "mu:uuid"
      ],
      optionalProperties: [
        "skos:prefLabel"
      ],
      additionalFilter: ""
    },
    {
      type: "ext:TaxType",
      requiredProperties: [
        "mu:uuid"
      ],
      optionalProperties: [
        "skos:prefLabel"
      ],
      additionalFilter: ""
    },
    {
      type: "ext:ChartOfAccount",
      requiredProperties: [
        "mu:uuid"
      ],
      optionalProperties: [
        "skos:prefLabel"
      ],
      additionalFilter: ""
    },
    {
      type: "skos:ConceptScheme",
      requiredProperties: [
        "mu:uuid"
      ],
      optionalProperties: [
        "skos:prefLabel"
      ],
      additionalFilter: ""
    },
    {
      type: "skos:Concept",
      requiredProperties: [
        "mu:uuid"
      ],
      optionalProperties: [
        "skos:prefLabel",
        "skos:inScheme",
        "skos:topConceptOf",
        "skos:notation"
      ],
      additionalFilter: `
?resource skos:inScheme ?conceptScheme .
FILTER (?conceptScheme IN (
  <http://lblod.data.gift/concept-schemes/5cecec47-ba66-4d7a-ac9d-a1e7962ca4e2>,
  <http://lblod.data.gift/concept-schemes/ac9bc402-c8e6-41fd-ad57-fad15622e560>,
  <https://data.vlaanderen.be/id/conceptscheme/BesluitType>,
  <https://data.vlaanderen.be/id/conceptscheme/BesluitDocumentType>,
  <http://lblod.data.gift/concept-schemes/b65b15ba-6755-4cd2-bd07-2c2cf3c0e4d3>,
  <http://lblod.data.gift/concept-schemes/c93ccd41-aee7-488f-86d3-038de890d05a>,
  <http://lblod.data.gift/concept-schemes/ac9bc402-c8e6-41fd-ad57-fad15622e560>,
  <http://lblod.data.gift/concept-schemes/71e6455e-1204-46a6-abf4-87319f58eaa5>))
      `
    },
    {
      type: "melding:FormData",
      requiredProperties: [
        "mu:uuid"
      ],
      optionalProperties: [
        "eli:date_publication",
        "elod:financialYear",
        "dct:description",
        "dct:relation",
        "rdfs:comment",
        "eli:first_date_entry_in_force",
        "eli:date_no_longer_in_force",
        "eli:is_about",
        "eli:passed_by"
      ],
      additionalFilter: `
BIND (?resource AS ?formData)
${ADVANCED_SUBMISSION_FILTER}
      `,
      hasDateFilter: true,
    },
    {
      type: "melding:FormData",
      requiredProperties: [
        "mu:uuid"
      ],
      optionalProperties: [
        "lblodBesluit:hasAdditionalTaxRate",
        "lblodBesluit:authenticityType",
        "lblodBesluit:chartOfAccount",
        "lblodBesluit:taxRate",
        "ext:sessionStartedAtTime"
      ],
      additionalFilter: `
BIND (?resource AS ?formData)
${ADVANCED_SUBMISSION_FILTER}
      `,
      hasDateFilter: true,
    },
    {
      type: "melding:FormData",
      requiredProperties: [
        "mu:uuid"
      ],
      optionalProperties: [
        "dct:hasPart",
        "ext:taxRateAmount",
        "dct:type",
        "dct:relation",
        "ext:decisionType",
        "ext:regulationType",
        "ext:taxType"
      ],
      additionalFilter: `
BIND (?resource AS ?formData)
${ADVANCED_SUBMISSION_FILTER}
      `,
      hasDateFilter: true,
    },
    {
      type: "nfo:FileDataObject",
      requiredProperties: [
        "mu:uuid"
      ],
      optionalProperties: [
        "nfo:fileName",
        "dct:format",
        "nfo:fileSize",
        "dbpedia:fileExtension",
        "dct:created",
        "dct:modified"
      ],
      additionalFilter: `
?submission
  nie:hasPart ?resource ;
  a meb:Submission ;
  nmo:sentDate ?sentDate .
      `,
      hasDateFilter: true,
    },
    {
      type: "nfo:FileDataObject",
      requiredProperties: [
        "mu:uuid"
      ],
      optionalProperties: [
        "nfo:fileName",
        "dct:format",
        "nfo:fileSize",
        "dbpedia:fileExtension",
        "dct:created",
        "dct:modified",
        "dct:type",
        "nie:dataSource"
      ],
      additionalFilter: `
?resource nie:dataSource ?remoteFile .
?submission
  a meb:Submission ;
  nie:hasPart ?remoteFile ;
  nmo:sentDate ?sentDate .
      `,
      hasDateFilter: true,
    },
    {
      type: "nfo:FileDataObject",
      requiredProperties: [
        "mu:uuid"
      ],
      optionalProperties: [
        "nfo:fileName",
        "dct:format",
        "nfo:fileSize",
        "dbpedia:fileExtension",
        "dct:created",
        "dct:modified",
        "dct:type",
        "nie:dataSource"
      ],
      additionalFilter: `
?resource nie:dataSource/nie:dataSource ?remoteFile .
?submission
  a meb:Submission ;
  nie:hasPart ?remoteFile ;
  nmo:sentDate ?sentDate .
      `,
      hasDateFilter: true,
    },
    {
      type: "nfo:FileDataObject",
      requiredProperties: [
        "mu:uuid"
      ],
      optionalProperties: [
        "nfo:fileName",
        "dct:format",
        "nfo:fileSize",
        "dbpedia:fileExtension",
        "dct:created",
        "dct:modified",
        "dct:type"
      ],
      additionalFilter: `
?resource dct:type <http://data.lblod.gift/concepts/meta-file-type> .
?submissionDocument
  dct:source ?resource ;
  a ext:SubmissionDocument .
?submission
  dct:subject ?submissionDocument ;
  a meb:Submission ;
  nmo:sentDate ?sentDate .
      `,
      hasDateFilter: true,
    },
    {
      type: "nfo:FileDataObject",
      requiredProperties: [
        "mu:uuid"
      ],
      optionalProperties: [
        "nfo:fileName",
        "dct:format",
        "nfo:fileSize",
        "dbpedia:fileExtension",
        "dct:created",
        "dct:modified",
        "dct:type"
      ],
      additionalFilter: `
?resource dct:type <http://data.lblod.gift/concepts/form-data-file-type> .
?submissionDocument
  dct:source ?resource ;
  a ext:SubmissionDocument .
?submission
  dct:subject ?submissionDocument ;
  a meb:Submission ;
  nmo:sentDate ?sentDate .
      `,
      hasDateFilter: true,
    },
    {
      type: "nfo:FileDataObject",
      requiredProperties: [
        "mu:uuid"
      ],
      optionalProperties: [
        "nfo:fileName",
        "dct:format",
        "nfo:fileSize",
        "dbpedia:fileExtension",
        "dct:created",
        "dct:modified",
        "dct:type"
      ],
      additionalFilter: `
?resource dct:type <http://data.lblod.gift/concepts/form-file-type> .
?submissionDocument
  dct:source ?resource ;
  a ext:SubmissionDocument .
?submission
  dct:subject ?submissionDocument ;
  a meb:Submission ;
  nmo:sentDate ?sentDate .
      `,
      hasDateFilter: true,
    },
    {
      type: "nfo:FileDataObject",
      requiredProperties: [
        "mu:uuid"
      ],
      optionalProperties: [
        "rdf:type",
        "nfo:fileName",
        "dct:format",
        "nfo:fileSize",
        "dbpedia:fileExtension",
        "dct:created",
        "dct:modified",
        "dct:type",
        "nie:dataSource"
      ],
      additionalFilter: `
?formData
  dct:hasPart ?resource ;
  a melding:FormData .
?submission
  prov:generated ?formData ;
  a meb:Submission ;
  nmo:sentDate ?sentDate .
      `,
      hasDateFilter: true,
    },
    {
      type: "nfo:FileDataObject",
      requiredProperties: [
        "mu:uuid"
      ],
      optionalProperties: [
        "rdf:type",
        "nie:url",
        "nfo:fileName",
        "dct:format",
        "nfo:fileSize",
        "dbpedia:fileExtension",
        "dct:created",
        "dct:modified",
        "dct:type",
        "nie:dataSource"
      ],
      additionalFilter: `
?resource nie:dataSource ?virtualFile .
?formData
  dct:hasPart ?virtualFile ;
  a melding:FormData .
?submission
  prov:generated ?formData ;
  a meb:Submission ;
  nmo:sentDate ?sentDate .
      `,
      hasDateFilter: true,
    },
    {
      type: "nfo:RemoteDataObject",
      requiredProperties: [
        "mu:uuid"
      ],
      optionalProperties: [
        "rdf:type",
        "nie:url",
        "nfo:fileName",
        "dct:format",
        "nfo:fileSize",
        "dbpedia:fileExtension",
        "dct:created",
        "dct:modified",
        "dct:type",
        "nie:dataSource",
        "adms:status"
      ],
      additionalFilter: `
?formData
  dct:hasPart ?resource ;
  a melding:FormData .
?submission
  prov:generated ?formData ;
  a meb:Submission ;
  nmo:sentDate ?sentDate .
      `,
      hasDateFilter: true,
    }
  ]
};
