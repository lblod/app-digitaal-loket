import { generateReportFromData, batchedQuery } from "../helpers.js";
import { getSafeValue } from "./util/report-helpers";

export default {
  cronPattern: "0 50 23 * * *",
  name: "eInclusionRequestReport",
  execute: async () => {
    const reportData = {
      title: "List of e-inclusion subsidies request",
      description: "E-Inclusion subsidy forms for request",
      filePrefix: "eInclusionSubsidyRequestReport",
    };

    const queryString = `
      PREFIX foaf: <http://xmlns.com/foaf/0.1/>
      PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
      PREFIX subsidie: <http://data.vlaanderen.be/ns/subsidie#>
      PREFIX lblodSubsidie: <http://lblod.data.gift/vocabularies/subsidie/>
      PREFIX transactie: <http://data.vlaanderen.be/ns/transactie#>
      PREFIX besluit: <http://data.vlaanderen.be/ns/besluit#>
      PREFIX mobiliteit: <https://data.vlaanderen.be/ns/mobiliteit#>
      PREFIX m8g: <http://data.europa.eu/m8g/>
      PREFIX dct: <http://purl.org/dc/terms/>
      PREFIX purl: <http://purl.org/vocab/cpsv#>
      PREFIX xkos: <http://rdf-vocabulary.ddialliance.org/xkos#>
      PREFIX adms: <http://www.w3.org/ns/adms#>
      PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
      PREFIX schema: <http://schema.org/>
      PREFIX ukraine: <http://lblod.data.gift/vocabularies/subsidie/ukraine/>

      SELECT DISTINCT ?subsidie ?modified ?status ?bestuurseenheid ?contactFirstName ?contactLastName
                      ?contactTelephone ?contactEmail ?rekeningnummer
      WHERE {
        ?subsidie a subsidie:SubsidiemaatregelConsumptie ;
                  transactie:isInstantieVan <http://lblod.data.info/id/subsidy-measure-offers/0b5cae58-97fb-4982-9fb7-4cf660f003df> ;
                  dct:source ?form .

        ?form dct:isPartOf/dct:references <http://data.lblod.info/id/subsidy-procedural-steps/4fec426a-37ce-41f3-b9b3-74d0f9349856>;
              adms:status/skos:prefLabel ?status .
          
        ?subsidie m8g:hasParticipation ?participation.


        ?bestuur m8g:playsRole ?participation ;
                skos:prefLabel ?bestuurseenheid.


        ?form dct:modified ?modified.

        OPTIONAL {
          ?form schema:contactPoint ?contactPoint .
          ?contactPoint foaf:firstName ?contactFirstName .
          ?contactPoint foaf:familyName ?contactLastName .
          ?contactPoint schema:email ?contactEmail .
          ?contactPoint schema:telephone ?contactTelephone .
        }

        OPTIONAL { 
          ?form schema:bankAccount/schema:identifier ?rekeningnummer .
        }
      }
      ORDER BY DESC(?modified)
    `;

    const queryResponse = await batchedQuery(queryString);
    const data = queryResponse.results.bindings.map((subsidie) => {
      return {
        subsidieURI: getSafeValue(subsidie, "subsidie"),
        modified: getSafeValue(subsidie, "modified"),
        status: getSafeValue(subsidie, "status"),
        bestuurseenheid: getSafeValue(subsidie, "bestuurseenheid"),
        contactFirstName: getSafeValue(subsidie, "contactFirstName"),
        contactLastName: getSafeValue(subsidie, "contactLastName"),
        contactTelephone: getSafeValue(subsidie, "contactTelephone"),
        contactEmail: getSafeValue(subsidie, "contactEmail"),
        rekeningnummer: getSafeValue(subsidie, "rekeningnummer"),
      };
    });

    await generateReportFromData(
      data,
      [
        "subsidieURI",
        "modified",
        "status",
        "bestuurseenheid",
        "contactFirstName",
        "contactLastName",
        "contactTelephone",
        "contactEmail",
        "rekeningnummer",
      ],
      reportData
    );
  },
};
