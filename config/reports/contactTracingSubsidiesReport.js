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

      SELECT DISTINCT ?submissionDate ?bestuurseenheid ?rekeningnummer ?bestaandPersoneelskader
              ?extraBestaandPersoneelskader ?vrijwilligers ?specifiekeUitgaven ?subsidiemaatregelConsumptie
      WHERE {
        ?subsidiemaatregelConsumptie
          transactie:isInstantieVan <http://lblod.data.gift/concepts/2697fbe1-4226-4325-807b-5dfa58e40a95> ;
          adms:status <http://lblod.data.gift/concepts/2ea29fbf-6d46-4f08-9343-879282a9f484> ;
          dct:source ?applicationForm ;
          dct:modified ?submissionDate ;
          m8g:hasParticipation ?participation .
        ?bestuur m8g:playsRole ?participation ;
          skos:prefLabel ?bestuurseenheid .
        ?applicationForm schema:bankAccount/schema:identifier ?rekeningnummer ;
          lblodSubsidie:engagementTable/ext:engagementEntry/ext:existingStaff ?bestaandPersoneelskader ;
          lblodSubsidie:engagementTable/ext:engagementEntry/ext:additionalStaff ?extraBestaandPersoneelskader ;
          lblodSubsidie:engagementTable/ext:engagementEntry/ext:volunteers ?vrijwilligers .

        OPTIONAL { ?applicationForm lblodSubsidie:estimatedExtraCosts ?specifiekeUitgaven . }
      }
      ORDER BY DESC(?submissionDate)
    `;
    const queryResponse = await query(queryString);
    const data = queryResponse.results.bindings.map((subsidie) => {
      return {
        submissionDate: subsidie.submissionDate.value,
        bestuurseenheid: subsidie.bestuurseenheid.value,
        rekeningnummer: subsidie.rekeningnummer.value,
        bestaandPersoneelskader: subsidie.bestaandPersoneelskader.value,
        extraBestaandPersoneelskader: subsidie.extraBestaandPersoneelskader.value,
        vrijwilligers: subsidie.vrijwilligers.value,
        specifiekeUitgaven: getSafeValue(subsidie, 'specifiekeUitgaven'),
        subsidiemaatregelConsumptie: subsidie.subsidiemaatregelConsumptie.value
      };
    });

    await generateReportFromData(data, [
      'submissionDate',
      'bestuurseenheid',
      'rekeningnummer',
      'bestaandPersoneelskader',
      'extraBestaandPersoneelskader',
      'vrijwilligers',
      'specifiekeUitgaven',
      'subsidiemaatregelConsumptie'
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
