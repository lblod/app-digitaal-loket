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
            (?reeksLabel as ?reeks) ?reeksStart ?reeksEnd ?createdByName ?modifiedByName ?subsidiemaatregelConsumptie
      WHERE {
        ?subsidiemaatregelConsumptie
          transactie:isInstantieVan <http://lblod.data.gift/concepts/1df4b56a-3ccd-450d-93dc-317fda1ada38> ;
          adms:status <http://lblod.data.gift/concepts/2ea29fbf-6d46-4f08-9343-879282a9f484> ;
          cpsv:follows ?applicationFlow ;
          dct:source ?applicationForm ;
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

        ?applicationForm
          schema:bankAccount/schema:identifier ?accountNumber ;
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
          ?applicationForm lblodSubsidie:totalAmount ?amount .
        }
        BIND(IF(BOUND(?amount), ?amount, xsd:float(0)) as ?totaalBedrag)
      }
      ORDER BY DESC(?aanvraagdatum)
    `;
    const queryResponse = await query(queryString);
    const data = queryResponse.results.bindings.map((subsidie) => {
      return {
        aanvraagdatum: subsidie.aanvraagdatum.value,
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
        aantalKinderenPerInfrastructuur: subsidie.aantalKinderenPerInfrastructuur.value,
        totaalBedrag: subsidie.totaalBedrag.value.toString().replace('.', ','),
        reeks: subsidie.reeks.value,
        reeksStart: subsidie.reeksStart.value,
        reeksEnd: subsidie.reeksEnd.value,
        createdByName: subsidie.createdByName.value,
        modifiedByName: subsidie.modifiedByName.value,
        subsidiemaatregelConsumptie: subsidie.subsidiemaatregelConsumptie.value
      };
    });

    await generateReportFromData(data, [
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
      'modifiedByName',
      'subsidiemaatregelConsumptie'
    ], reportData);
  }
};
