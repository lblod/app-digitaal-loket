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
                    ?contactTelephone ?contactEmail ?rekeningnummer ?jobTitle ?amount 
                    group_concat(distinct ?action;separator=" & ") as ?actions 
                    ?goal ?description 
                    group_concat(distinct ?targetedAudience;separator=" & ") as ?targetedAudiences
                    ?otherAudience 
                    ?additionalType ?additionalGoal ?additionalDescription
                    group_concat(distinct ?additionalTargetedAudience;separator=" & ") as ?additionalTargetedAudiences
                    ?additionalOtherAudience {
                        
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
            OPTIONAL { ?contactPoint foaf:firstName ?contactFirstName . }
            OPTIONAL { ?contactPoint foaf:familyName ?contactLastName . }
            OPTIONAL { ?contactPoint schema:email ?contactEmail . }
            OPTIONAL { ?contactPoint schema:telephone ?contactTelephone . }
            OPTIONAL { ?contactPoint schema:jobTitle ?jobTitle . }
        }
      
        OPTIONAL { 
            ?form schema:bankAccount/schema:identifier ?rekeningnummer .
        }
      
        OPTIONAL { ?form lblodSubsidie:amount ?amount . }
        OPTIONAL { ?form lblodSubsidie:actionShortDescription ?goal . }
        OPTIONAL { ?form lblodSubsidie:actionFullDescription ?description . }
        OPTIONAL { ?form lblodSubsidie:targetedAudienceOther ?otherAudience . }
        OPTIONAL {
          ?form lblodSubsidie:currentEInclusionActions ?tempActions.
          ?tempActions skos:prefLabel ?action.
        }
        OPTIONAL {
            ?form lblodSubsidie:targetedAudience ?tempAudience.
            ?tempAudience skos:prefLabel ?targetedAudience.
        }
        OPTIONAL {
            ?form lblodSubsidie:hasAdditionalAction ?additionalAction.
            OPTIONAL {
                OPTIONAL { ?additionalAction lblodSubsidie:additionalActionType ?tempAdditionalType .
                        ?tempAdditionalType skos:prefLabel ?additionalType . }
                OPTIONAL { ?additionalAction lblodSubsidie:additionalActionShortDescription ?additionalGoal . }
                OPTIONAL { ?additionalAction lblodSubsidie:additionalActionFullDescription ?additionalDescription . }
                OPTIONAL { ?additionalAction lblodSubsidie:additionalActionTargetedAudience ?tempAdditionalTargetedAudience.
                    ?tempAdditionalTargetedAudience skos:prefLabel ?additionalTargetedAudience . }
                OPTIONAL { ?additionalAction lblodSubsidie:additionalActionTargetedAudienceOther ?additionalOtherAudience . }
            }
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
        contactFunction: getSafeValue(subsidie, "jobTitle"),
        rekeningnummer: getSafeValue(subsidie, "rekeningnummer"),
        gevraagdSubsidiebedrag: getSafeValue(subsidie, "amount"),
        geselecteerdeActies: getSafeValue(subsidie, "actions"),
        doelstelling: getSafeValue(subsidie, "goal"),
        beschrijvingActie: getSafeValue(subsidie, "description"),
        geselecteerdeDoelgroepen: getSafeValue(subsidie, "targetedAudiences"),
        andere_bijkomendeDoelgroepen: getSafeValue(subsidie, "otherAudience"),
        bijkomende_geselecteerdeActies: getSafeValue(subsidie, "additionalType"),
        bijkomende_doelstelling: getSafeValue(subsidie, "additionalGoal"),
        bijkomende_beschrijvingActie: getSafeValue(subsidie, "additionalDescription"),
        bijkomende_geselecteerdeDoelgroepen: getSafeValue(subsidie, "additionalTargetedAudiences"),
        bijkomendeDoelgroepen: getSafeValue(subsidie, "additionalOtherAudience"),
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
        "contactFunction",
        "rekeningnummer",
        "gevraagdSubsidiebedrag",
        "geselecteerdeActies",
        "doelstelling",
        "beschrijvingActie",
        "geselecteerdeDoelgroepen",
        "andere_bijkomendeDoelgroepen",
        "bijkomende_geselecteerdeActies",
        "bijkomende_doelstelling",
        "bijkomende_beschrijvingActie",
        "bijkomende_geselecteerdeDoelgroepen",
        "bijkomendeDoelgroepen",
      ],
      reportData
    );
  },
};
