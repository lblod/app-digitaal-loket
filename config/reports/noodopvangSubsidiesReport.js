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
      PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
      PREFIX lblodSubsidie: <http://lblod.data.gift/vocabularies/subsidie/>
      PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
      PREFIX prov: <http://www.w3.org/ns/prov#>
      PREFIX schema: <http://schema.org/>
      PREFIX subsidie: <http://data.vlaanderen.be/ns/subsidie#>
      PREFIX transactie: <http://data.vlaanderen.be/ns/transactie#>
      PREFIX dct: <http://purl.org/dc/terms/>
      PREFIX pav: <http://purl.org/pav/>
      PREFIX adms: <http://www.w3.org/ns/adms#>
      PREFIX gleif: <https://www.gleif.org/ontology/Base/>

      SELECT DISTINCT ?aanvraagdatum ?bedrag (?bestuurseenheidLabel as ?bestuurseenheid)
      ?contactFirstName ?contactLastName ?contactEmail ?contactTelephone ?accountNumber
      ?aantalUniekeKinderen ?aantalKalenderdagen ?naamOrganisator ?aantalKinderenVoorAlleVolleDagen
      ?aantalKinderenVoorAlleHalveDagen ?aantalKinderenInAanmerking
      (?reeksLabel as ?reeks) ?reeksStart ?reeksEnd ?createdByName ?modifiedByName ?subsidie
      WHERE {
        ?subsidie a lblodSubsidie:ApplicationForm ;
          subsidie:aanvraagdatum ?aanvraagdatum ;
          pav:createdBy ?bestuurseenheid ;
          dct:creator ?createdBy ;
          ext:lastModifiedBy ?modifiedBy ;
          lblodSubsidie:timeBlock ?reeks ;
          schema:contactPoint ?contactPoint ;
          schema:bankAccount ?bankAccount ;
          lblodSubsidie:uniqueChildrenNumberForWholePeriod ?aantalUniekeKinderen  ;
          lblodSubsidie:daysOfChildcareForWholePeriod ?aantalKalenderdagen ;
          lblodSubsidie:applicationFormTable ?formTable ;
          lblodSubsidie:subsidyMeasure <http://lblod.data.gift/concepts/1df4b56a-3ccd-450d-93dc-317fda1ada38> ;
          adms:status <http://lblod.data.gift/concepts/9bd8d86d-bb10-4456-a84e-91e9507c374c> .

        ?bestuurseenheid skos:prefLabel ?bestuurseenheidLabel .

        ?reeks skos:prefLabel ?reeksLabel ;
          gleif:hasStart ?reeksStart ;
          gleif:hasEnd ?reeksEnd ;
          ext:submissionPeriod ?submissionPeriod .

        ?createdBy foaf:firstName ?createdByFirstName ;
          foaf:familyName ?createdByLastName .

        BIND(CONCAT(?createdByFirstName, " ", ?createdByLastName) as ?createdByName)

        ?modifiedBy foaf:firstName ?modifiedByFirstName ;
          foaf:familyName ?modifiedByLastName .

        BIND(CONCAT(?modifiedByFirstName, " ", ?modifiedByLastName) as ?modifiedByName)

        ?contactPoint foaf:firstName ?contactFirstName ;
          foaf:familyName ?contactLastName ;
          schema:email ?contactEmail ;
          schema:telephone ?contactTelephone .

        ?bankAccount schema:identifier ?accountNumber .

        ?formTable ext:applicationFormEntry ?formEntry .

        ?formEntry ext:actorName ?naamOrganisator ;
          ext:numberChildrenForFullDay ?aantalKinderenVoorAlleVolleDagen ;
          ext:numberChildrenForHalfDay ?aantalKinderenVoorAlleHalveDagen ;
          ext:numberChildrenPerInfrastructure ?aantalKinderenInAanmerking  .


        OPTIONAL {
          ?subsidie lblodSubsidie:totalAmount ?amount .
        }
        BIND(IF(BOUND(?amount), ?amount, xsd:float(0)) as ?bedrag)
      }
      ORDER BY DESC(?aanvraagdatum)
    `;
    const queryResponse = await query(queryString);
    const data = queryResponse.results.bindings.map((subsidie) => {
      return {
        aanvraagdatum: subsidie.aanvraagdatum.value,
        bedrag: subsidie.bedrag.value,
        bestuurseenheid: subsidie.bestuurseenheid.value,
        contactFirstName: subsidie.contactFirstName.value,
        contactLastName: subsidie.contactLastName.value,
        contactEmail: subsidie.contactEmail.value,
        contactTelephone: subsidie.contactTelephone.value,
        aantalUniekeKinderen: subsidie.aantalUniekeKinderen.value,
        aantalKalenderdagen: subsidie.aantalKalenderdagen.value,
        accountNumber: subsidie.accountNumber.value,
        naamOrganisator: subsidie.naamOrganisator.value,
        aantalKinderenVoorAlleVolleDagen: subsidie.aantalKinderenVoorAlleVolleDagen.value,
        aantalKinderenVoorAlleHalveDagen: subsidie.aantalKinderenVoorAlleHalveDagen.value,
        aantalKinderenInAanmerking: subsidie.aantalKinderenInAanmerking.value,
        reeks: subsidie.reeks.value,
        reeksStart: subsidie.reeksStart.value,
        reeksEnd: subsidie.reeksEnd.value,
        createdByName: subsidie.createdByName.value,
        modifiedByName: subsidie.modifiedByName.value,
        subsidie: subsidie.subsidie.value
      };
    });

     await generateReportFromData(data, [
       'aanvraagdatum',
       'bedrag',
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
       'aantalKinderenInAanmerking',
       'reeks',
       'reeksStart',
       'reeksEnd',
       'createdByName',
       'modifiedByName',
       'subsidie'
      ], reportData);
  }
};
