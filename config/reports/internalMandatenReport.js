import {generateReportFromData, batchedQuery} from '../helpers.js';

export default {
  cronPattern: '0 0 4 * * 0',
  name: 'internalMandatenReport',
  execute: async () => {
    const reportData = {
      title: 'Mandatendatabank CSV',
      description: 'CSV data dump of Mandatendatabank',
      filePrefix: 'internalMandatenReport'
    };
    console.log('Generate Internal Mandaten Report');
    const queryStringPart1 = `
      PREFIX mandaat: <http://data.vlaanderen.be/ns/mandaat#>
      PREFIX besluit: <http://data.vlaanderen.be/ns/besluit#>
      PREFIX persoon: <http://data.vlaanderen.be/ns/persoon#>
      PREFIX foaf: <http://xmlns.com/foaf/0.1/>
      PREFIX org: <http://www.w3.org/ns/org#>
      PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
      PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
      PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
      PREFIX regorg: <https://www.w3.org/ns/regorg#>

      SELECT DISTINCT (?s AS ?mandataris) ?start ?eind ?rangorde ?status WHERE {
        ?s a mandaat:Mandataris .
        ?s org:holds ?mandaat .
        ?mandaat ^org:hasPost ?bestuursorgaanInTijd .
        ?bestuursorgaanInTijd mandaat:bindingStart ?bestuursPeriodeStart .
        OPTIONAL { ?s mandaat:start ?start . }
        OPTIONAL { ?s mandaat:einde ?eind . }
        OPTIONAL { ?s mandaat:rangorde ?rangorde . }
        OPTIONAL { ?s mandaat:status ?status . }
        FILTER (?bestuursPeriodeStart >= "2019-01-01"^^<http://www.w3.org/2001/XMLSchema#date>)

      }
    `;

    const queryResponsePart1 = await batchedQuery(queryStringPart1);
    const dataPart1 = queryResponsePart1.results.bindings.reduce( (acc, row) => {
      acc[row?.mandataris?.value] = {
        mandataris: row?.mandataris?.value,
        start: row?.start?.value,
        eind: row?.eind?.value,
        rangorde: row?.rangorde?.value,
        status: row?.status?.value
      };
      return acc;
    }, {});

    const queryStringPart2 = `
      PREFIX mandaat: <http://data.vlaanderen.be/ns/mandaat#>
      PREFIX besluit: <http://data.vlaanderen.be/ns/besluit#>
      PREFIX persoon: <http://data.vlaanderen.be/ns/persoon#>
      PREFIX foaf: <http://xmlns.com/foaf/0.1/>
      PREFIX org: <http://www.w3.org/ns/org#>
      PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
      PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
      PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
      PREFIX regorg: <https://www.w3.org/ns/regorg#>
      SELECT DISTINCT (?s AS ?mandataris) ?persoon ?voornaam ?achternaam ?roepnaam ?geslacht ?geboortedatum WHERE {
        ?s a mandaat:Mandataris .
        ?s mandaat:isBestuurlijkeAliasVan ?persoon .
        ?s org:holds ?mandaat .
        ?mandaat ^org:hasPost ?bestuursorgaanInTijd .
        ?bestuursorgaanInTijd mandaat:bindingStart ?bestuursPeriodeStart .

        OPTIONAL { ?persoon foaf:familyName ?achternaam . }
        OPTIONAL { ?persoon persoon:gebruikteVoornaam ?voornaam . }
        OPTIONAL { ?persoon foaf:name ?roepnaam . }
        OPTIONAL { ?persoon persoon:geslacht ?geslacht }
        OPTIONAL { ?persoon persoon:heeftGeboorte/persoon:datum ?geboortedatum }

        FILTER (?bestuursPeriodeStart >= "2019-01-01"^^<http://www.w3.org/2001/XMLSchema#date>)
      }
    `;

    const queryResponsePart2 = await batchedQuery(queryStringPart2);
    const dataPart2 = queryResponsePart2.results.bindings.reduce( (acc, row) => {
      let dataPart = {
        mandataris: row?.mandataris?.value,
        persoon: row?.persoon?.value,
        voornaam: row?.voornaam?.value,
        achternaam: row?.achternaam?.value,
        roepnaam: row?.roepnaam?.value,
        geslacht: row?.geslacht?.value,
        geboortedatum: row?.geboortedatum?.value
      };
      acc[row?.mandataris?.value] = Object.assign(dataPart, dataPart1[row?.mandataris?.value]);
      return acc;
    }, {});

    const queryStringPart3 = `
      PREFIX mandaat: <http://data.vlaanderen.be/ns/mandaat#>
      PREFIX besluit: <http://data.vlaanderen.be/ns/besluit#>
      PREFIX persoon: <http://data.vlaanderen.be/ns/persoon#>
      PREFIX foaf: <http://xmlns.com/foaf/0.1/>
      PREFIX org: <http://www.w3.org/ns/org#>
      PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
      PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
      PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
      PREFIX regorg: <https://www.w3.org/ns/regorg#>

      SELECT DISTINCT (?s AS ?mandataris) ?bestuursfunctieLabel ?bestuursorgaanLabel ?bestuursorgaanClassificatieLabel ?bestuurseenheidLabel ?bestuurseenheidClassificatieLabel ?werkingsgebiedLabel ?werkingsgebiedNiveau ?bestuursPeriodeStart ?bestuursPeriodeEinde WHERE {
        ?s a mandaat:Mandataris .
        ?s org:holds ?mandaat .
        ?mandaat ^org:hasPost ?bestuursorgaanInTijd .
        ?bestuursorgaanInTijd mandaat:bindingStart ?bestuursPeriodeStart .
        ?s org:holds ?mandaat .
        ?mandaat org:role ?bestuursfunctie .

       ?bestuursorgaanInTijd mandaat:isTijdspecialisatieVan ?bestuursorgaan .
       ?bestuursorgaan skos:prefLabel ?bestuursorgaanLabel .
       ?bestuursorgaan besluit:bestuurt ?bestuurseenheid .
       ?bestuurseenheid skos:prefLabel ?bestuurseenheidLabel .
       ?bestuurseenheid besluit:classificatie ?bestuurseenheidClassificatie .
       ?bestuurseenheidClassificatie skos:prefLabel ?bestuurseenheidClassificatieLabel .
       ?bestuursorgaan besluit:classificatie ?bestuursorgaanClassificatie .
       ?bestuursorgaanClassificatie skos:prefLabel ?bestuursorgaanClassificatieLabel.
       OPTIONAL {
                ?bestuurseenheid besluit:werkingsgebied ?werkingsgebied .
                OPTIONAL { ?werkingsgebied rdfs:label ?werkingsgebiedLabel . }
                OPTIONAL { ?werkingsgebied ext:werkingsgebiedNiveau ?werkingsgebiedNiveau . }
        }
        OPTIONAL {
          ?bestuursfunctie skos:prefLabel ?bestuursfunctieLabel .
        }

        OPTIONAL { ?bestuursorgaanInTijd mandaat:bindingEinde ?bestuursPeriodeEinde }
        FILTER (?bestuursPeriodeStart >= "2019-01-01"^^<http://www.w3.org/2001/XMLSchema#date>)
      }
    `;
    const queryResponsePart3 = await batchedQuery(queryStringPart3);

    const dataPart3 = queryResponsePart3.results.bindings.reduce( (acc, row) => {
      let dataPart = {
        bestuursfunctieLabel: row?.bestuursfunctieLabel?.value,
        bestuursorgaanLabel: row?.bestuursorgaanLabel?.value,
        bestuursorgaanClassificatieLabel: row?.bestuursorgaanClassificatieLabel?.value,
        bestuurseenheidLabel: row?.bestuurseenheidLabel?.value,
        bestuurseenheidClassificatieLabel: row?.bestuurseenheidClassificatieLabel?.value,
        werkingsgebiedLabel: row?.werkingsgebiedLabel?.value,
        werkingsgebiedNiveau: row?.werkingsgebiedNiveau?.value,
        bestuursPeriodeStart: row?.bestuursPeriodeStart?.value,
        bestuursPeriodeEinde: row?.bestuursPeriodeEinde?.value
      };
      acc[row?.mandataris?.value] = Object.assign(dataPart, dataPart2[row?.mandataris?.value]);
      return acc;
    }, {});


    const queryStringPart4 = `
      PREFIX mandaat: <http://data.vlaanderen.be/ns/mandaat#>
      PREFIX besluit: <http://data.vlaanderen.be/ns/besluit#>
      PREFIX persoon: <http://data.vlaanderen.be/ns/persoon#>
      PREFIX foaf: <http://xmlns.com/foaf/0.1/>
      PREFIX org: <http://www.w3.org/ns/org#>
      PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
      PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
      PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
      PREFIX regorg: <https://www.w3.org/ns/regorg#>

      SELECT DISTINCT (?s AS ?mandataris) ?fractieNaam WHERE {
        ?s a mandaat:Mandataris .
        ?s mandaat:isBestuurlijkeAliasVan ?persoon .
        ?s org:holds ?mandaat .
        ?mandaat ^org:hasPost ?bestuursorgaanInTijd .
        ?bestuursorgaanInTijd mandaat:bindingStart ?bestuursPeriodeStart .

        OPTIONAL {
          ?s org:hasMembership/org:organisation ?fractie .
          OPTIONAL { ?fractie regorg:legalName ?fractieNaam . }
        }
        FILTER (?bestuursPeriodeStart >= "2019-01-01"^^<http://www.w3.org/2001/XMLSchema#date>)
      }
    `;

  const queryResponsePart4 = await batchedQuery(queryStringPart4);
    const dataPart4 = queryResponsePart4.results.bindings.reduce( (acc, row) => {
      let dataPart = {
        fractieNaam: row?.fractieNaam?.value
      };
      acc[row?.mandataris?.value] = Object.assign(dataPart, dataPart3[row?.mandataris?.value]);
      return acc;
    }, {});

    await generateReportFromData(Object.values(dataPart4), [
      'mandataris', 'start', 'eind', 'rangorde', 'status', 'persoon', 'voornaam', 'achternaam', 'roepnaam',
      'bestuursfunctieLabel', 'bestuursorgaanLabel', 'bestuursorgaanClassificatieLabel', 'bestuurseenheidLabel',
      'bestuurseenheidClassificatieLabel', 'werkingsgebiedLabel', 'werkingsgebiedNiveau', 'bestuursPeriodeStart', 'bestuursPeriodeEinde', 'fractieNaam', 'geslacht',
      'geboortedatum'
    ], reportData);
  }
};
