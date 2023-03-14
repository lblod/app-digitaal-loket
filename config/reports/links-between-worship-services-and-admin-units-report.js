import { generateReportFromData, batchedQuery } from '../helpers.js';

export default {
  cronPattern: '0 44 23 * * 0',
  name: 'linksBetweenWorshipServicesAndAdminUnitsReport',
  execute: async () => {
    const reportData = {
      title: `Links between worship services and non-religious administrative units`,
      description: `All the links that this application has between worship services and administrative units (ROs, municipalities and provinces), used to dispatch submissions.`,
      filePrefix: `link-between-worship-service-and-admin-units-report`,
    };
    console.log('Links between worship services and admin units report');

    /**
     * Query that extracts all links bewteen ROs and worship services.
     *
     * The query exists out of the union of two queries:
     * - The link between ROs and worship services via org:linkedTo
     * - The link between municipalities or provinces and worship services via local involvments
     **/
    const queryString = `
      PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
      PREFIX org: <http://www.w3.org/ns/org#>
      PREFIX besluit: <http://data.vlaanderen.be/ns/besluit#>
      PREFIX ere: <http://data.lblod.info/vocabularies/erediensten/>
      
      SELECT DISTINCT ?worshipAdministrativeUnit ?worshipLabel ?worshipClassification ?adminUnit ?adminUnitLabel ?adminUnitClassification ?typeLocalInvolvment
      WHERE {
        VALUES ?type {
          ere:BestuurVanDeEredienst
          ere:CentraalBestuurVanDeEredienst
        }
      
        ?worshipAdministrativeUnit a ?type ;
          skos:prefLabel ?worshipLabel ;
          besluit:classificatie/skos:prefLabel ?worshipClassification .
      
        {
          ?adminUnit org:linkedTo ?worshipAdministrativeUnit ;
            besluit:classificatie/skos:prefLabel ?adminUnitClassification ;
            skos:prefLabel ?adminUnitLabel .
        }
        UNION
        {
          VALUES ?classification {
            <http://data.vlaanderen.be/id/concept/BestuurseenheidClassificatieCode/5ab0e9b8a3b2ca7c5e000000>
            <http://data.vlaanderen.be/id/concept/BestuurseenheidClassificatieCode/5ab0e9b8a3b2ca7c5e000001>
          }
      
          ?betrokke org:organization ?worshipAdministrativeUnit ;
            ere:typebetrokkenheid/skos:prefLabel ?tmpTypeLocalInvolvment .
      
          ?adminUnit a besluit:Bestuurseenheid ;
            skos:prefLabel ?adminUnitLabel ;
            besluit:classificatie ?classification ;
            ere:betrokkenBestuur ?betrokke .
      
          ?classification skos:prefLabel ?adminUnitClassification .
        }
      
        BIND(IF(BOUND(?tmpTypeLocalInvolvment), ?tmpTypeLocalInvolvment, "N/A") AS ?typeLocalInvolvment)
      }
      ORDER BY ?worshipAdministrativeUnit
    `;
    const queryResponse = await batchedQuery(queryString);
    const data = queryResponse.results.bindings.map((unit) => {
      return {
        worshipAdministrativeUnit: unit.worshipAdministrativeUnit.value,
        worshipLabel: unit.worshipLabel.value,
        worshipClassification: unit.worshipClassification.value,
        adminUnit: unit.adminUnit.value,
        adminUnitLabel: unit.adminUnitLabel.value,
        adminUnitClassification: unit.adminUnitClassification.value,
        typeLocalInvolvment: unit.typeLocalInvolvment.value,
      };
    });

    await generateReportFromData(data, [
      'worshipAdministrativeUnit',
      'worshipLabel',
      'worshipClassification',
      'adminUnit',
      'adminUnitLabel',
      'adminUnitClassification',
      'typeLocalInvolvment'
    ], reportData);
  }
};
