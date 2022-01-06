import { generateReportFromData, batchedQuery } from '../helpers.js';

export default {
  cronPattern: '0 0 0 * * *',
  name: 'gzgSubsidieOproepTwoReport',
  execute: async () => {
    const reportData = {
      title: 'List of GzG Report oproep 3, step 1',
      description: 'GzG subsidy forms of oproep 3, step 1',
      filePrefix: 'gzgSubsidieOproepThreeReport'
    };

    const queryStringPart1 = `
      PREFIX foaf: <http://xmlns.com/foaf/0.1/>
      PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
      PREFIX subsidie: <http://data.vlaanderen.be/ns/subsidie#>
      PREFIX lblodSubsidie: <http://lblod.data.gift/vocabularies/subsidie/>
      PREFIX transactie: <http://data.vlaanderen.be/ns/transactie#>
      PREFIX besluit: <http://data.vlaanderen.be/ns/besluit#>
      PREFIX m8g: <http://data.europa.eu/m8g/>
      PREFIX dct: <http://purl.org/dc/terms/>
      PREFIX adms: <http://www.w3.org/ns/adms#>
      PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
      PREFIX schema: <http://schema.org/>

      SELECT DISTINCT ?subsidie ?aanvraagdatum ?bestuurseenheid ?contactFirstName ?contactLastName
                      ?contactTelephone ?contactEmail ?projectNaam
                      ?projectStartDatum ?projectEindDatum ?aanvraagBedrag ?thema ?aangemaaktDoor
                      ?gewijzigdDoor ?subsidiemaatregelConsumptieStatus ?stepOneFormStatus
      WHERE {
        {
          ?subsidie a subsidie:SubsidiemaatregelConsumptie ;
            transactie:isInstantieVan <http://lblod.data.info/id/subsidy-measure-offers/8379a4ea-fd83-47cc-89fa-1a72ee4fbaff> ;
            adms:status/skos:prefLabel ?subsidiemaatregelConsumptieStatus ;
            dct:source ?form .

          ?form dct:isPartOf/dct:references <http://data.lblod.info/id/subsidy-procedural-steps/f61dc47f-e7f8-46f3-b37d-27730c440ec0> ;
            adms:status ?status;
            adms:status/skos:prefLabel ?stepOneFormStatus .

           FILTER(?status IN (<http://lblod.data.gift/concepts/9bd8d86d-bb10-4456-a84e-91e9507c374c>,
                              <http://lblod.data.gift/concepts/79a52da4-f491-4e2f-9374-89a13cde8ecd>))

          OPTIONAL {
            ?subsidie m8g:hasParticipation ?participation ;
            dct:modified ?aanvraagdatum ;
            dct:creator ?creator ;
            ext:lastModifiedBy ?lastModified.

            ?bestuur m8g:playsRole ?participation ;
                    skos:prefLabel ?bestuurseenheid .

            ?participation m8g:role <http://lblod.data.gift/concepts/d8b8f3d1-7574-4baf-94df-188a7bd84a3a>.

            ?creator foaf:firstName ?creatorNaam.
            ?creator foaf:familyName ?creatorFamilienaam.

            BIND(CONCAT(?creatorNaam, " ", ?creatorFamilienaam) as ?aangemaaktDoor)

            ?lastModified foaf:firstName ?modifierNaam.
            ?lastModified foaf:familyName ?modifierFamilienaam.

            BIND(CONCAT(?modifierNaam, " ", ?modifierFamilienaam) as ?gewijzigdDoor)

            ?form dct:modified ?modified.

            ?form schema:contactPoint ?contactPoint .
            ?contactPoint foaf:firstName ?contactFirstName .
            ?contactPoint foaf:familyName ?contactLastName .
            ?contactPoint schema:email ?contactEmail .
            ?contactPoint schema:telephone ?contactTelephone .

            ?form lblodSubsidie:projectName ?projectNaam.
            ?form lblodSubsidie:projectStartDate ?projectStartDatum.
            ?form lblodSubsidie:projectEndDate ?projectEindDatum.
            ?form lblodSubsidie:totalAmount ?aanvraagBedrag.
            ?form lblodSubsidie:projectType/skos:prefLabel ?thema.
          }
        }
        UNION
        {
          ?subsidie a subsidie:SubsidiemaatregelConsumptie ;
            transactie:isInstantieVan <http://lblod.data.info/id/subsidy-measure-offers/8379a4ea-fd83-47cc-89fa-1a72ee4fbaff> .
          FILTER NOT EXISTS {
            ?subsidie dct:source ?anyForm.
            ?anyForm dct:isPartOf ?step.
          }
        }
      }
      ORDER BY DESC(?aanvraagdatum)
    `;

    const queryResponsePart1 = await batchedQuery(queryStringPart1);
    const dataPart1 = queryResponsePart1.results.bindings.reduce( (acc, row) => {
      acc[getSafeValue(row, 'subsidie')] = {
          subsidie: getSafeValue(row, 'subsidie'),
          aanvraagdatum: getSafeValue(row, 'aanvraagdatum'),
          bestuurseenheid: getSafeValue(row, 'bestuurseenheid'),
          contactFirstName: getSafeValue(row, 'contactFirstName'),
          contactLastName: getSafeValue(row, 'contactLastName'),
          contactEmail: getSafeValue(row, 'contactEmail'),
          contactTelephone: getSafeValue(row, 'contactTelephone'),
          projectNaam: getSafeValue(row, 'projectNaam'),
          projectStartDatum: getSafeValue(row, 'projectStartDatum'),
          projectEindDatum: getSafeValue(row, 'projectEindDatum'),
          aanvraagBedrag: getSafeValue(row, 'aanvraagBedrag'),
          thema: getSafeValue(row, 'thema'),
          aangemaaktDoor: getSafeValue(row, 'aangemaaktDoor'),
          gewijzigdDoor: getSafeValue(row, 'gewijzigdDoor'),
          subsidiemaatregelConsumptieStatus: getSafeValue(row, 'subsidiemaatregelConsumptieStatus'),
          stepOneFormStatus: getSafeValue(row, 'stepOneFormStatus'),
      };
      return acc;
    }, {});

    const queryStringPart2 = `
      PREFIX foaf: <http://xmlns.com/foaf/0.1/>
      PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
      PREFIX subsidie: <http://data.vlaanderen.be/ns/subsidie#>
      PREFIX lblodSubsidie: <http://lblod.data.gift/vocabularies/subsidie/>
      PREFIX transactie: <http://data.vlaanderen.be/ns/transactie#>
      PREFIX besluit: <http://data.vlaanderen.be/ns/besluit#>
      PREFIX m8g: <http://data.europa.eu/m8g/>
      PREFIX dct: <http://purl.org/dc/terms/>
      PREFIX adms: <http://www.w3.org/ns/adms#>
      PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
      PREFIX schema: <http://schema.org/>

      SELECT DISTINCT ?subsidie ?isSamenwerkingsverband
                      (group_concat(DISTINCT ?samenwerkingsverband;separator=";") as ?samenwerkingsverbanden)
      WHERE {
        {
          ?subsidie a subsidie:SubsidiemaatregelConsumptie ;
            transactie:isInstantieVan <http://lblod.data.info/id/subsidy-measure-offers/8379a4ea-fd83-47cc-89fa-1a72ee4fbaff> ;
            dct:source ?form .

          ?form dct:isPartOf/dct:references <http://data.lblod.info/id/subsidy-procedural-steps/f61dc47f-e7f8-46f3-b37d-27730c440ec0>> ;
            adms:status ?status .

           FILTER(?status IN (<http://lblod.data.gift/concepts/9bd8d86d-bb10-4456-a84e-91e9507c374c>,
                              <http://lblod.data.gift/concepts/79a52da4-f491-4e2f-9374-89a13cde8ecd>))


          OPTIONAL {
            ?subsidie dct:modified ?aanvraagdatum .

            ?form lblodSubsidie:isCollaboration/skos:prefLabel ?isSamenwerkingsverband .
            OPTIONAL { ?form lblodSubsidie:collaborator/skos:prefLabel ?samenwerkingsverband . }

            FILTER EXISTS { ?form adms:status <http://lblod.data.gift/concepts/9bd8d86d-bb10-4456-a84e-91e9507c374c> }
          }
        }
        UNION
        {
          ?subsidie a subsidie:SubsidiemaatregelConsumptie ;
            transactie:isInstantieVan <http://lblod.data.info/id/subsidy-measure-offers/8379a4ea-fd83-47cc-89fa-1a72ee4fbaff> .
          FILTER NOT EXISTS {
            ?subsidie dct:source ?anyForm.
            ?anyForm dct:isPartOf ?step.
          }
        }
      }
      GROUP BY ?subsidie ?aanvraagdatum ?form ?isSamenwerkingsverband
      ORDER BY DESC(?aanvraagdatum)
    `;

    const queryResponsePart2 = await batchedQuery(queryStringPart2);
    const dataPart2 = queryResponsePart2.results.bindings.reduce( (acc, row) => {
      let dataPart = {
        isSamenwerkingsverband: getSafeValue(row, 'isSamenwerkingsverband'),
        samenwerkingsverbanden: getSafeValue(row, 'samenwerkingsverbanden')
      };
      acc[getSafeValue(row, 'subsidie')] = Object.assign(dataPart, dataPart1[getSafeValue(row, 'subsidie')]);
      return acc;
    }, {});

    await generateReportFromData(Object.values(dataPart2), [
      'subsidie',
      'aanvraagdatum',
      'bestuurseenheid',
      'contactFirstName',
      'contactLastName',
      'contactTelephone',
      'contactEmail',
      'isSamenwerkingsverband',
      'samenwerkingsverbanden',
      'projectNaam',
      'projectStartDatum',
      'projectEindDatum',
      'aanvraagBedrag',
      'thema',
      'aangemaaktDoor',
      'gewijzigdDoor',
      'subsidiemaatregelConsumptieStatus',
      'stepOneFormStatus',
    ], reportData);
  }
};

function getSafeValue(entry, property){
  return entry[property] ? wrapInQuote(entry[property].value) : null;
}

// Some values might contain comas, wrapping them in escapes quotes doesn't disturb the colomns
function wrapInQuote(value) {
  return `\"${value}\"`;
}
