import {generateReportFromData, batchedQuery} from '../helpers.js';

export default {
  cronPattern: '0 15 23 * * 0',
  name: 'personsMissingDataReport',
  execute: async () => {
    const reportData = {
      title: 'Persons missing data CSV',
      description: 'Persons who are missing at least one of the following : national number, birth date, gender',
      filePrefix: 'personsMissingData'
    };
    console.log('Generate Persons Missing Data Report');

    const queryStringPart1 = `
      PREFIX person: <http://www.w3.org/ns/person#>
      PREFIX persoon: <http://data.vlaanderen.be/ns/persoon#>
      PREFIX foaf: <http://xmlns.com/foaf/0.1/>

      SELECT DISTINCT ?person ?firstName ?lastName
      WHERE {
        GRAPH ?g {
          ?person a person:Person ;
            persoon:gebruikteVoornaam ?firstName ;
            foaf:familyName ?lastName .
        }
      }
    `;

    const queryResponsePart1 = await batchedQuery(queryStringPart1);

    const dataPart1 = queryResponsePart1.results.bindings.reduce( (acc, row) => {
      acc[row?.person?.value] = {
        person: row?.person?.value,
        firstName: row?.firstName?.value,
        lastName: row?.lastName?.value
      };
      return acc;
    }, {});

    const queryStringPart2 = `
      PREFIX mandaat: <http://data.vlaanderen.be/ns/mandaat#>

      SELECT DISTINCT ?person ?mandataris
      WHERE { 
        GRAPH ?g {
          ?mandataris a mandaat:Mandataris ;
            mandaat:isBestuurlijkeAliasVan ?person .
        }
      }
    `;

    const queryResponsePart2 = await batchedQuery(queryStringPart2);
    const dataPart2 = queryResponsePart2.results.bindings.reduce( (acc, row) => {
      let dataPart = {
        person: row?.person?.value,
        mandataris: row?.mandataris?.value
      };

      if(acc[row?.person?.value] && acc[row?.person?.value].mandataris) {
        dataPart.mandataris = `${acc[row?.person?.value].mandataris} ---and--- ${dataPart.mandataris}`;
      }

      acc[row?.person?.value] = Object.assign(dataPart, dataPart1[row?.person?.value]);
      return acc;
    }, {});

    const queryStringPart3 = `
      PREFIX mandaat: <http://data.vlaanderen.be/ns/mandaat#>
      PREFIX person: <http://www.w3.org/ns/person#>
      PREFIX persoon: <http://data.vlaanderen.be/ns/persoon#>
      PREFIX adms: <http://www.w3.org/ns/adms#>
      PREFIX foaf: <http://xmlns.com/foaf/0.1/>

      SELECT DISTINCT ?person ?mandataris ?gender ?birthDate
      WHERE { 
        GRAPH ?g {
          ?person a person:Person .
          OPTIONAL {
            ?person persoon:geslacht ?gender .
          }
          OPTIONAL {
            ?person persoon:heeftGeboorte ?birthDateUri .
            ?birthDateUri persoon:datum ?birthDate .
          }
          FILTER ( NOT EXISTS { ?person adms:identifier ?identifier } || NOT EXISTS { ?person persoon:geslacht ?gender . } || NOT EXISTS { ?person persoon:heeftGeboorte ?birthDateUri . } )
        }
      }
    `;

    const queryResponsePart3 = await batchedQuery(queryStringPart3);
    const dataPart3 = queryResponsePart3.results.bindings.reduce( (acc, row) => {
      let dataPart = {
        person: row?.person?.value,
        gender: row?.gender?.value,
        birthDate: row?.birthDate?.value
      };
      acc[row?.person?.value] = Object.assign(dataPart, dataPart2[row?.person?.value] || dataPart1[row?.person?.value]); // dataPart2 can be empty if no mandataris found
      return acc;
    }, {});

    await generateReportFromData(Object.values(dataPart3), [
      'person', 'mandataris', 'gender', 'birthDate', 'firstName', 'lastName'
    ], reportData);
  }
};
