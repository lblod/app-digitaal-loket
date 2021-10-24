import {generateReportFromData} from '../helpers.js';
import { querySudo as query } from '@lblod/mu-auth-sudo';

export default {
  cronPattern: '0 11 23 * * *',
  name: 'noodopvangSubsidiesReport',
  execute: async () => {
    const reportData = {
      title: 'Lijst van noodopvang subsidies',
      description: 'All subsidies about emergency childcare that have been sent with their related information',
      filePrefix: 'noodopvangSubsidiesReport'
    };
    console.log('Generate Subsidies Report');
    const queryString = `
      PREFIX prov: <http://www.w3.org/ns/prov#>
      PREFIX subsidie: <http://data.vlaanderen.be/ns/subsidie#>
      PREFIX transactie: <http://data.vlaanderen.be/ns/transactie#>
      PREFIX m8g: <http://data.europa.eu/m8g/>
      PREFIX schema: <http://schema.org/>
      PREFIX dct: <http://purl.org/dc/terms/>
      PREFIX lblodSubsidie: <http://lblod.data.gift/vocabularies/subsidie/>
      PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
      PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
      PREFIX pav: <http://purl.org/pav/>
      PREFIX adms: <http://www.w3.org/ns/adms#>
      PREFIX mobiliteit: <https://data.vlaanderen.be/ns/mobiliteit#>
      PREFIX cpsv: <http://purl.org/vocab/cpsv#>
      PREFIX xkos: <http://rdf-vocabulary.ddialliance.org/xkos#>

      SELECT DISTINCT ?aanvraagdatum ?bestuurseenheid
            ?contactFirstName ?contactLastName ?contactEmail ?contactTelephone ?accountNumber
            ?aantalUniekeKinderen ?aantalKalenderdagen ?naamOrganisator ?aantalKinderenVoorAlleVolleDagen
            ?aantalKinderenVoorAlleHalveDagen ?aantalKinderenPerInfrastructuur ?totaalBedrag
            (?reeksLabel as ?reeks) ?reeksStart ?reeksEnd ?createdByName ?modifiedByName ?subsidie
      WHERE {
        {
          ?subsidie a subsidie:SubsidiemaatregelConsumptie ;
            transactie:isInstantieVan <http://lblod.data.gift/concepts/1df4b56a-3ccd-450d-93dc-317fda1ada38> ;
            adms:status <http://lblod.data.gift/concepts/2ea29fbf-6d46-4f08-9343-879282a9f484> ;
            dct:source ?form .
          ?form dct:isPartOf/dct:references ?references ;
            adms:status <http://lblod.data.gift/concepts/9bd8d86d-bb10-4456-a84e-91e9507c374c> .

          VALUES ?references {
            <http://data.lblod.info/id/subsidieprocedurestappen/aabe45ba93f31abbcb488cb090aac0158241c5f08e61cb41e1497ad386010aeb>
            <http://data.lblod.info/id/subsidieprocedurestappen/4beae4a11ddb49c381c22745a090327cffe5693c60d2f20ac13a8acccb58db7a>
            <http://data.lblod.info/id/subsidieprocedurestappen/67fb349694b6fa141bea4ad3212bbae0db995b967152736aea304958dd9f7d72>
            <http://data.lblod.info/id/subsidieprocedurestappen/7028ed3808e8d2dcb68abe1ecb44609c5a1a0971d6dfbee42fc14350c87742be>
            <http://data.lblod.info/id/subsidieprocedurestappen/8bee70b3f7b762d48c57944302d0897f8360bed5d8d49f32264772d6ebf8bef7>
            <http://data.lblod.info/id/subsidieprocedurestappen/7da2c107da4c8fc3aaad374da2dd5392a8f88b2f1e192ce3813cee16038d6a09>
            <http://data.lblod.info/id/subsidieprocedurestappen/d87de1deddc1bd6f9c8119c1e43dd26688e08ad56aa2e5c0fc287ecf0e202ba7>
          }

          OPTIONAL {
            ?subsidie cpsv:follows ?applicationFlow ;
              dct:modified ?aanvraagdatum ;
              m8g:hasParticipation ?participation ;
              ext:lastModifiedBy ?modifiedBy ;
              dct:creator ?createdBy .

            ?applicationFlow xkos:belongsTo ?measureOfferSeries .

            ?measureOfferSeries dct:title ?reeksLabel ;
              mobiliteit:periode/m8g:startTime ?reeksStart ;
              mobiliteit:periode/m8g:endTime ?reeksEnd .

            ?bestuur m8g:playsRole ?participation ;
              skos:prefLabel ?bestuurseenheid .

            ?form schema:bankAccount/schema:identifier ?accountNumber ;
              schema:contactPoint ?contactPoint ;
              lblodSubsidie:uniqueChildrenNumberForWholePeriod ?aantalUniekeKinderen  ;
              lblodSubsidie:daysOfChildcareForWholePeriod ?aantalKalenderdagen ;
              lblodSubsidie:applicationFormTable ?formTable .

            ?contactPoint foaf:firstName ?contactFirstName ;
              foaf:familyName ?contactLastName ;
              schema:email ?contactEmail ;
              schema:telephone ?contactTelephone .

            ?formTable ext:applicationFormEntry ?formEntry .

            ?formEntry ext:actorName ?naamOrganisator ;
              ext:numberChildrenForFullDay ?aantalKinderenVoorAlleVolleDagen ;
              ext:numberChildrenForHalfDay ?aantalKinderenVoorAlleHalveDagen ;
              ext:numberChildrenPerInfrastructure ?aantalKinderenPerInfrastructuur  .

            ?createdBy foaf:firstName ?createdByFirstName ;
              foaf:familyName ?createdByLastName .

            BIND(CONCAT(?createdByFirstName, " ", ?createdByLastName) as ?createdByName)

            ?modifiedBy foaf:firstName ?modifiedByFirstName ;
              foaf:familyName ?modifiedByLastName .

            BIND(CONCAT(?modifiedByFirstName, " ", ?modifiedByLastName) as ?modifiedByName)

            OPTIONAL {
              ?form lblodSubsidie:totalAmount ?amount .
            }
            BIND(IF(BOUND(?amount), ?amount, xsd:float(0)) as ?totaalBedrag)
          }
        }
        UNION
        {
          ?subsidie a subsidie:SubsidiemaatregelConsumptie ;
            transactie:isInstantieVan <http://lblod.data.gift/concepts/1df4b56a-3ccd-450d-93dc-317fda1ada38> .
          FILTER NOT EXISTS {
            ?subsidie dct:source ?anyForm.
            ?anyForm dct:isPartOf ?step.
          }
        }
      }
      ORDER BY DESC(?aanvraagdatum)
    `;
    const queryResponse = await query(queryString);
    const data = queryResponse.results.bindings.map((subsidie) => {
      return {
        aanvraagdatum: getSafeValue(subsidie, 'aanvraagdatum'),
        bestuurseenheid: getSafeValue(subsidie, 'bestuurseenheid'),
        contactFirstName: getSafeValue(subsidie, 'contactFirstName'),
        contactLastName: getSafeValue(subsidie, 'contactLastName'),
        contactEmail: getSafeValue(subsidie, 'contactEmail'),
        contactTelephone: getSafeValue(subsidie, 'contactTelephone'),
        aantalUniekeKinderen: getSafeValue(subsidie, 'aantalUniekeKinderen'),
        aantalKalenderdagen: getSafeValue(subsidie, 'aantalKalenderdagen'),
        accountNumber: getSafeValue(subsidie, 'accountNumber'),
        naamOrganisator: getSafeValue(subsidie, 'naamOrganisator'),
        aantalKinderenVoorAlleVolleDagen: getSafeValue(subsidie, 'aantalKinderenVoorAlleVolleDagen'),
        aantalKinderenVoorAlleHalveDagen: getSafeValue(subsidie, 'aantalKinderenVoorAlleHalveDagen'),
        aantalKinderenPerInfrastructuur: getSafeValue(subsidie, 'aantalKinderenPerInfrastructuur'),
        totaalBedrag: getSafeValue(subsidie, 'totaalBedrag'),
        reeks: getSafeValue(subsidie, 'reeks'),
        reeksStart: getSafeValue(subsidie, 'reeksStart'),
        reeksEnd: getSafeValue(subsidie, 'reeksEnd'),
        createdByName: getSafeValue(subsidie, 'createdByName'),
        modifiedByName: getSafeValue(subsidie, 'modifiedByName'),
        subsidie: getSafeValue(subsidie, 'subsidie'),
      };
    });

    await generateReportFromData(data, [
      'subsidie',
      'aanvraagdatum',
      'bestuurseenheid',
      'contactFirstName',
      'contactLastName',
      'contactEmail',
      'contactTelephone',
      'aantalUniekeKinderen',
      'aantalKalenderdagen',
      'accountNumber',
      'naamOrganisator',
      'aantalKinderenVoorAlleVolleDagen',
      'aantalKinderenVoorAlleHalveDagen',
      'aantalKinderenVoorAlleHalveDagen',
      'aantalKinderenPerInfrastructuur',
      'totaalBedrag',
      'reeks',
      'reeksStart',
      'reeksEnd',
      'createdByName',
      'modifiedByName'
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
