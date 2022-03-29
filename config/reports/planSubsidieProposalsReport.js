import { generateReportFromData } from '../helpers.js';
import { querySudo as query } from '@lblod/mu-auth-sudo';

export default {
  cronPattern: '0 30 1 * * *',
  name: 'planSubsidieProposalsReport',
  execute: async () => {
    const reportData = {
      title: 'List of proposals of plan samenleven subsidies',
      description: 'All proposals for plan living together subsidies that have been sent with their related information',
      filePrefix: 'planSubsidieProposalsReport'
    };
    console.log('Generate Plan Samenleven Subsidie Proposals Report');
    const queryString = `
      PREFIX foaf: <http://xmlns.com/foaf/0.1/>
      PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
      PREFIX subsidie: <http://data.vlaanderen.be/ns/subsidie#>
      PREFIX lblodSubsidie: <http://lblod.data.gift/vocabularies/subsidie/>
      PREFIX transactie: <http://data.vlaanderen.be/ns/transactie#>
      PREFIX besluit: <http://data.vlaanderen.be/ns/besluit#>
      PREFIX m8g: <http://data.europa.eu/m8g/>
      PREFIX dct: <http://purl.org/dc/terms/>
      PREFIX adms: <http://www.w3.org/ns/adms#>
      PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
      PREFIX schema: <http://schema.org/>
      PREFIX planSamenleven: <http://lblod.data.gift/vocabularies/subsidie/plan-samenleven/>

      SELECT DISTINCT ?subsidie ?modified ?status ?contactFirstName ?contactLastName
                      ?contactTelephone ?contactEmail ?rekeningnummer ?bestuurseenheid ?kbo ?classificatie ?samenwerking
                      ?socialImpactBond ?voorstelSocialImpactBond ?mentorschap ?mentorschapActie ?mentorschapMotivering
      WHERE {
        ?subsidie a subsidie:SubsidiemaatregelConsumptie ;
                  transactie:isInstantieVan <http://lblod.data.info/id/subsidy-measure-offers/a2d920b2-d266-4555-b023-63632997c406> ;
                  dct:source ?form .

        ?form dct:isPartOf/dct:references <http://data.lblod.info/id/subsidy-procedural-steps/42d5b4d0-458f-4dcc-93e0-9a6cfd44a6f0> ;
              adms:status/skos:prefLabel ?status .
          
        ?subsidie m8g:hasParticipation ?participation.

        ?bestuur m8g:playsRole ?participation ;
                skos:prefLabel ?bestuurseenheid ;
                ext:kbonummer ?kbo;
                besluit:classificatie/skos:prefLabel ?classificatie.


        ?form dct:modified ?modified.

        ?form schema:contactPoint ?contactPoint .
        ?contactPoint foaf:firstName ?contactFirstName .
        ?contactPoint foaf:familyName ?contactLastName .
        ?contactPoint schema:email ?contactEmail .
        ?contactPoint schema:telephone ?contactTelephone .

        ?form schema:bankAccount/schema:identifier ?rekeningnummer .

        ?form planSamenleven:hasCollaborationWithOtherLBNonEu/skos:prefLabel ?samenwerking;
              planSamenleven:usesSocialImpactBound/skos:prefLabel ?socialImpactBond;
              planSamenleven:proposesMentorship/skos:prefLabel ?mentorschap.

        OPTIONAL {
          ?form planSamenleven:mentorshipAction ?mentorschapActie.
        }

        OPTIONAL {
          ?form planSamenleven:usesSocialImpactBoundProposal ?voorstelSocialImpactBond.
        }

        OPTIONAL {
          ?form planSamenleven:mentorshipActionDetails ?mentorschapMotivering.
        }
      }
    `;

    const queryResponse = await query(queryString);
    const data = queryResponse.results.bindings.map((subsidie) => {
      return {
        subsidieURI: getSafeValue(subsidie, 'subsidie'),
        modified: getSafeValue(subsidie, 'modified'),
        contactFirstName: getSafeValue(subsidie, 'contactFirstName'),
        contactLastName: getSafeValue(subsidie, 'contactLastName'),
        contactTelephone: getSafeValue(subsidie, 'contactTelephone'),
        contactEmail: getSafeValue(subsidie, 'contactEmail'),
        rekeningnummer: getSafeValue(subsidie, 'rekeningnummer'),
        bestuurseenheid: getSafeValue(subsidie, 'bestuurseenheid'),
        kbo: getSafeValue(subsidie, 'kbo'),
        classificatie: getSafeValue(subsidie, 'classificatie'),
        samenwerking: getSafeValue(subsidie, 'samenwerking'),
        socialImpactBond: getSafeValue(subsidie, 'socialImpactBond'),
        voorstelSocialImpactBond: getSafeValue(subsidie, 'voorstelSocialImpactBond'),
        mentorschap: getSafeValue(subsidie, 'mentorschap'),
        mentorschapActie: getSafeValue(subsidie, 'mentorschapActie'),
        mentorschapMotivering: getSafeValue(subsidie, 'mentorschapMotivering')
      };
    });

    await generateReportFromData(data, [
      'subsidieURI',
      'modified',
      'contactFirstName',
      'contactLastName',
      'contactTelephone',
      'contactEmail',
      'rekeningnummer',
      'bestuurseenheid',
      'kbo',
      'classificatie',
      'samenwerking',
      'socialImpactBond',
      'voorstelSocialImpactBond',
      'mentorschap',
      'mentorschapActie',
      'mentorschapMotivering'
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
