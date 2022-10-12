import { generateReportFromData } from '../helpers.js';
import { querySudo as query } from '@lblod/mu-auth-sudo';

export default {
  cronPattern: '0 42 23 * * *',
  name: 'contactTracingSubsidiesReport',
  execute: async () => {
    const reportData = {
      title: 'List of contact tracing subsidies',
      description: 'All contact tracing subsidies that have been sent with their related information',
      filePrefix: 'contactTracingSubsidiesReport'
    };
    console.log('Generate Fiets Subsidies Report');
    const queryString = `
      PREFIX prov: <http://www.w3.org/ns/prov#>
      PREFIX subsidie: <http://data.vlaanderen.be/ns/subsidie#>
      PREFIX transactie: <http://data.vlaanderen.be/ns/transactie#>
      PREFIX m8g: <http://data.europa.eu/m8g/>
      PREFIX schema: <http://schema.org/>
      PREFIX dct: <http://purl.org/dc/terms/>
      PREFIX lblodSubsidie: <http://lblod.data.gift/vocabularies/subsidie/>
      PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
      PREFIX adms: <http://www.w3.org/ns/adms#>
      PREFIX besluit: <http://data.vlaanderen.be/ns/besluit#>

      SELECT DISTINCT ?submissionDate ?bestuurseenheid ?kbo ?classificatie ?rekeningnummer
                      ?bestaandPersoneelskader ?extraBestaandPersoneelskader ?vrijwilligers
                      ?specifiekeUitgaven ?subsidie
      WHERE {
        {
          ?subsidie a subsidie:SubsidiemaatregelConsumptie ;
            transactie:isInstantieVan <http://lblod.data.gift/concepts/2697fbe1-4226-4325-807b-5dfa58e40a95> ;
            adms:status ?status ;
            dct:source ?form .
          ?form dct:isPartOf/dct:references ?references ;
            adms:status <http://lblod.data.gift/concepts/9bd8d86d-bb10-4456-a84e-91e9507c374c> .

          VALUES ?references {
            <http://data.lblod.info/id/subsidieprocedurestappen/84464f95b2278b3394d0178478d8772d9dde89480827bfddf49db25a677c9491>
            <http://data.lblod.info/id/subsidy-procedural-steps/6e6b1f8a-0758-42e8-95e6-bec36e04864e>
          }

          OPTIONAL {
            ?subsidie dct:modified ?submissionDate ;
              m8g:hasParticipation ?participation .
            ?bestuur m8g:playsRole ?participation ;
              skos:prefLabel ?bestuurseenheid ;
              ext:kbonummer ?kbo ;
              besluit:classificatie/skos:prefLabel ?classificatie .
            ?form schema:bankAccount/schema:identifier ?rekeningnummer ;
              lblodSubsidie:engagementTable/ext:engagementEntry/ext:existingStaff ?bestaandPersoneelskader ;
              lblodSubsidie:engagementTable/ext:engagementEntry/ext:additionalStaff ?extraBestaandPersoneelskader ;
              lblodSubsidie:engagementTable/ext:engagementEntry/ext:volunteers ?vrijwilligers .

            OPTIONAL { ?form lblodSubsidie:estimatedExtraCosts ?specifiekeUitgaven . }
          }
        }
        UNION
        {
          ?subsidie a subsidie:SubsidiemaatregelConsumptie ;
            transactie:isInstantieVan <http://lblod.data.gift/concepts/2697fbe1-4226-4325-807b-5dfa58e40a95> .
          FILTER NOT EXISTS {
            ?subsidie dct:source ?anyForm.
            ?anyForm dct:isPartOf ?step.
          }
        }
      }
      ORDER BY DESC(?submissionDate)
    `;
    const queryResponse = await query(queryString);
    const data = queryResponse.results.bindings.map((subsidie) => {
      return {
        submissionDate: getSafeValue(subsidie, 'submissionDate'),
        bestuurseenheid: getSafeValue(subsidie, 'bestuurseenheid'),
        kbo: getSafeValue(subsidie, 'kbo'),
        classificatie: getSafeValue(subsidie, 'classificatie'),
        rekeningnummer: getSafeValue(subsidie, 'rekeningnummer'),
        bestaandPersoneelskader: getSafeValue(subsidie, 'bestaandPersoneelskader'),
        extraBestaandPersoneelskader: getSafeValue(subsidie, 'extraBestaandPersoneelskader'),
        vrijwilligers: getSafeValue(subsidie, 'vrijwilligers'),
        specifiekeUitgaven: getSafeValue(subsidie, 'specifiekeUitgaven'),
        subsidie: getSafeValue(subsidie, 'subsidie'),
      };
    });

    await generateReportFromData(data, [
      'subsidie',
      'submissionDate',
      'bestuurseenheid',
      'kbo',
      'classificatie',
      'rekeningnummer',
      'bestaandPersoneelskader',
      'extraBestaandPersoneelskader',
      'vrijwilligers',
      'specifiekeUitgaven',
    ], reportData);
  }
};

function getSafeValue(entry, property) {
  return entry[property] ? wrapInQuote(entry[property].value) : null;
}

// Some values might contain comas, wrapping them in escapes quotes doesn't disturb the colomns
function wrapInQuote(value) {
  return `\"${value}\"`;
}
