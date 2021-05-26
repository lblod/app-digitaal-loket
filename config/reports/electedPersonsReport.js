import { generateReportFromData, batchedQuery } from '../helpers.js';

export default {
  cronPattern: '0 44 23 * * 0',
  name: 'electedPersonsReport',
  execute: async () => {
    const reportData = {
      title: 'Elected Persons CSV',
      description: 'Elected persons with following properties : uri, first and last names, rrn, bestuurseenheid and list',
      filePrefix: 'electedPersonsReport'
    };
    console.log('Generate Elected Persons Report');

    const queryString = `
      PREFIX person: <http://www.w3.org/ns/person#>
      PREFIX persoon: <http://data.vlaanderen.be/ns/persoon#>
      PREFIX foaf: <http://xmlns.com/foaf/0.1/>
      PREFIX mandaat: <http://data.vlaanderen.be/ns/mandaat#>
      PREFIX besluit: <http://data.vlaanderen.be/ns/besluit#>
      PREFIX adms: <http://www.w3.org/ns/adms#>

      SELECT DISTINCT ?person ?firstName ?lastName ?rrn (?kandidatenlijstLabel as ?kandidatenlijst) ?bestuurseenheid
      WHERE {
        GRAPH ?g {
          ?person a person:Person ;
            persoon:gebruikteVoornaam ?firstName ;
            foaf:familyName ?lastName ;
            adms:identifier/skos:notation ?rrn .
        }
        GRAPH ?h {
          ?kandidatenlijst mandaat:heeftKandidaat ?person ;
            skos:prefLabel ?kandidatenlijstLabel ;
            mandaat:behoortTot ?rechtstreekseVerkiezing .
          ?rechtstreekseVerkiezing mandaat:steltSamen ?bestuursorgaanInTijd .
          ?bestuursorgaanInTijd mandaat:isTijdspecialisatieVan ?bestuursorgaan .
          ?bestuursorgaan besluit:bestuurt/skos:prefLabel ?bestuurseenheid .
        }
      }
    `;
    const queryResponse = await batchedQuery(queryString);
    console.log()
    const data = queryResponse.results.bindings.map((person) => {
      return {
        person: person.person.value,
        firstName: person.firstName.value,
        lastName: person.lastName.value,
        rrn: person.rrn.value,
        kandidatenlijst: person.kandidatenlijst.value,
        bestuurseenheid: person.bestuurseenheid.value,
      };
    });

    await generateReportFromData(data, [
      'person',
      'firstName',
      'lastName',
      'rrn',
      'kandidatenlijst',
      'bestuurseenheid'
    ], reportData);
  }
};
