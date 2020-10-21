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
    const dataToEnrich = queryResponsePart1.results.bindings.map((row) => ({
      person: getSafeValue(row, 'person'),
      firstName: getSafeValue(row, 'firstName'),
      lastName: getSafeValue(row, 'lastName')
    }));
    enrichWithHomonyms(dataToEnrich);

    const dataPart1 = dataToEnrich.reduce( (acc, row) => {
      acc[row.person] = {
        person: row.person,
        firstName: row.firstName,
        lastName: row.lastName,
        homonyms: row.homonyms
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
        person: getSafeValue(row, 'person'),
        mandataris: getSafeValue(row, 'mandataris')
      };
      acc[getSafeValue(row, 'person')] = Object.assign(dataPart, dataPart1[getSafeValue(row, 'person')]);
      return acc;
    }, {});

    const queryStringPart3 = `
      PREFIX mandaat: <http://data.vlaanderen.be/ns/mandaat#>
      PREFIX person: <http://www.w3.org/ns/person#>
      PREFIX persoon: <http://data.vlaanderen.be/ns/persoon#>
      PREFIX adms: <http://www.w3.org/ns/adms#>
      PREFIX foaf: <http://xmlns.com/foaf/0.1/>

      SELECT DISTINCT ?person ?mandataris ?rrn ?gender ?birthDate
      WHERE { 
        GRAPH ?g {
          ?person a person:Person .
          OPTIONAL {
            ?person adms:identifier ?identifier .
            ?identifier skos:notation ?rrn .
          }
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
        person: getSafeValue(row, 'person'),
        rrn: getSafeValue(row, 'rrn'),
        gender: getSafeValue(row, 'gender'),
        birthDate: getSafeValue(row, 'birthDate')
      };
      acc[getSafeValue(row, 'person')] = Object.assign(dataPart, dataPart2[getSafeValue(row, 'person')]);
      return acc;
    }, {});

    await generateReportFromData(Object.values(dataPart3), [
      'person', 'mandataris', 'rrn', 'gender', 'birthDate', 'firstName', 'lastName', 'homonyms'
    ], reportData);
  }
};

function getSafeValue(entry, property){
  return entry[property] ? entry[property].value: null;
}

function enrichWithHomonyms(data) {
  data.forEach(line => {
    const homonyms = data.filter(homonym => (
      homonym.firstName != "" && homonym.firstName == line.firstName &&
      homonym.lastName != "" && homonym.lastName  == line.lastName &&
      homonym.person  != line.person)
    );

    if (homonyms && homonyms.length) {
      line.homonyms = homonyms.map(h => h.person).join(" ");
    }
  });
}