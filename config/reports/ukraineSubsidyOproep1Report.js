import { generateReportFromData, batchedQuery } from "../helpers.js";

export default {
  cronPattern: "0 30 23 * * *",
  name: "ukraineSubsidyOproepOneReport",
  execute: async () => {
    const reportData = {
      title: "List of Ukraine subsidies oproep 1",
      description: "Ukraine subsidy forms of oproep 1",
      filePrefix: "ukraineSubsidyOproepOneReport",
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

      SELECT DISTINCT ?subsidie ?modified ?status ?contactFirstName ?contactLastName
                      ?contactTelephone ?contactEmail ?rekeningnummer ?bestuurseenheid ?kbo ?classificatie ?reeks
                      ?reeksStart ?reeksEnd ?address ?aantalSlaapkamers ?facturenGemeenschDelen
      WHERE {
        ?subsidie a subsidie:SubsidiemaatregelConsumptie ;
                  transactie:isInstantieVan <http://lblod.data.info/id/subsidy-measure-offers/b03215cf-2206-493c-b534-0546a2479eef> ;
                  dct:source ?form .

        ?form dct:isPartOf/dct:references <http://data.lblod.info/id/subsidy-procedural-steps/7d9ffc75-29ee-45a6-8f7a-e2f88a81aab9> ;
              adms:status/skos:prefLabel ?status .
          
        ?subsidie m8g:hasParticipation ?participation.

        ?subsidie purl:follows ?x.
        ?x xkos:belongsTo ?reeksInfo.
        ?reeksInfo dct:title ?reeks;
                   mobiliteit:periode ?reeksperiode.
        ?reeksperiode m8g:startTime ?reeksStartDateTime;
                      m8g:endTime ?reeksEndDateTime.
        
        BIND(CONCAT(YEAR(?reeksStartDateTime), "-",MONTH(?reeksStartDateTime), "-", DAY(?reeksStartDateTime)) as ?reeksStart)
        BIND(CONCAT(YEAR(?reeksEndDateTime), "-",MONTH(?reeksEndDateTime), "-", DAY(?reeksEndDateTime)) as ?reeksEnd)  

        ?bestuur m8g:playsRole ?participation ;
                skos:prefLabel ?bestuurseenheid ;
                ext:kbonummer ?kbo;
                besluit:classificatie/skos:prefLabel ?classificatie.


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
 
        OPTIONAL {
          ?form lblodSubsidie:accountabilityTable ?table.
          OPTIONAL {
            ?table lblodSubsidie:accountabilityEntry ?entry.
            ?entry ukraine:address ?address;
                  ukraine:bedroomCount ?aantalSlaapkamers;
                  ukraine:sharedInvoice ?facturenGemeenschDelen.
          }
        }
      }
      ORDER BY DESC(?modified)
    `;

    const queryResponse = await batchedQuery(queryString);
    const data = queryResponse.results.bindings.map((subsidie) => {
      return {
        subsidieURI: subsidie?.subsidie?.value,
        modified: subsidie?.modified?.value,
        status: subsidie?.status?.value,
        bestuurseenheid: subsidie?.bestuurseenheid?.value,
        kbo: subsidie?.kbo?.value,
        classificatie: subsidie?.classificatie?.value,
        contactFirstName: subsidie?.contactFirstName?.value,
        contactLastName: subsidie?.contactLastName?.value,
        contactTelephone: subsidie?.contactTelephone?.value,
        contactEmail: subsidie?.contactEmail?.value,
        rekeningnummer: subsidie?.rekeningnummer?.value,
        reeks: subsidie?.reeks?.value,
        reeksStart: subsidie?.reeksStart?.value,
        reeksEnd: subsidie?.reeksEnd?.value,
        address: subsidie?.address?.value,
        aantalSlaapkamers: subsidie?.aantalSlaapkamers?.value,
        facturenGemeenschDelen: subsidie?.facturenGemeenschDelen?.value,
      };
    });

    await generateReportFromData(
      data,
      [
        "subsidieURI",
        "modified",
        "status",
        "bestuurseenheid",
        "kbo",
        "classificatie",
        "contactFirstName",
        "contactLastName",
        "contactTelephone",
        "contactEmail",
        "rekeningnummer",
        "reeks",
        "reeksStart",
        "reeksEnd",
        "address",
        "aantalSlaapkamers",
        "facturenGemeenschDelen",
      ],
      reportData
    );
  },
};
