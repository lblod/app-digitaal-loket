import {generateReportFromData} from '../helpers.js';
import { querySudo as query } from '@lblod/mu-auth-sudo';

export default {
  cronPattern: '0 0 23 * * *',
  name: 'mar7300InzendingenReport',
  execute: async () => {
    const reportData = {
      title: 'Inzendingen of type MAR7300',
      description: 'Information about inzendingen of type MAR7300',
      filePrefix: 'mar7300-inzendingen'
    };
    console.log('Generate MAR7300 Inzendingen Report');

    const queryString = `
      PREFIX toezicht: <http://mu.semte.ch/vocabularies/ext/supervision/>
      PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
      PREFIX nmo: <http://www.semanticdesktop.org/ontologies/2007/03/22/nmo#>
      PREFIX dct: <http://purl.org/dc/terms/>
      PREFIX prov: <http://www.w3.org/ns/prov#>
      PREFIX eli: <http://data.europa.eu/eli/ontology#>
      PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
      PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
      PREFIX lblodBesluit: <http://lblod.data.gift/vocabularies/besluit/>
      PREFIX mandaat: <http://data.vlaanderen.be/ns/mandaat#>
      PREFIX nie: <http://www.semanticdesktop.org/ontologies/2007/01/19/nie#>
      PREFIX nfo: <http://www.semanticdesktop.org/ontologies/2007/03/22/nfo#>
      PREFIX meb: <http://rdf.myexperiment.org/ontologies/base/>

      SELECT ?sentDate ?bestuursorgaan ?dateOfEntryInForce ?datePublication ?dateNoLongerInForce ?dateSessionStartedAtTime ?attachmentName ?hasAdditionalTaxRate ?taxRateAmount ?submission where {
        GRAPH ?g {
          ?submission a meb:Submission ;
            nmo:sentDate ?sentDate;
            prov:generated ?form .
          ?form ext:decisionType <https://data.vlaanderen.be/id/concept/BesluitType/67378dd0-5413-474b-8996-d992ef81637a> ;
            ext:regulationType <https://data.vlaanderen.be/id/concept/BesluitType/efa4ec5a-b006-453f-985f-f986ebae11bc> ;
            ext:taxType <https://data.vlaanderen.be/id/concept/BesluitType/b2d0734d-13d0-44b4-9af8-1722933c5288> ;
            eli:passed_by ?orgaanInTijd ;
            eli:first_date_entry_in_force ?dateOfEntryInForce ;
            eli:date_no_longer_in_force ?dateNoLongerInForce ;
            ext:sessionStartedAtTime ?dateSessionStartedAtTime ;
            dct:hasPart ?attachment .

          OPTIONAL { ?form eli:date_publication ?datePublication . }
          OPTIONAL { ?form ext:taxRateAmount ?taxRateAmount . }
          OPTIONAL { ?form lblodBesluit:hasAdditionalTaxRate ?hasAdditionalTaxRate . }
        }
        GRAPH ?h {
          ?orgaanInTijd mandaat:isTijdspecialisatieVan/skos:prefLabel ?bestuursorgaan .
        }
        GRAPH ?i {
          ?attachment nie:url|nfo:fileName ?attachmentName .
        }
      }
      ORDER BY DESC(?sentDate)
    `;
    const queryResponse = await query(queryString);
    const data = queryResponse.results.bindings.map((inzendingen) => ({
      sentDate: inzendingen.sentDate.value,
      bestuursorgaan: inzendingen.bestuursorgaan.value,
      dateOfEntryInForce: inzendingen.dateOfEntryInForce.value,
      datePublication: inzendingen.datePublication.value,
      dateNoLongerInForce: inzendingen.dateNoLongerInForce.value,
      dateSessionStartedAtTime: inzendingen.dateSessionStartedAtTime.value,
      attachmentName: inzendingen.attachmentName.value,
      hasAdditionalTaxRate: inzendingen.hasAdditionalTaxRate ? inzendingen.hasAdditionalTaxRate.value : '',
      taxRateAmount: inzendingen.taxRateAmount ? inzendingen.taxRateAmount.value : '',
      submission: inzendingen.submission.value
    }));
    await generateReportFromData(data, [
      'sentDate',
      'bestuursorgaan',
      'dateOfEntryInForce',
      'datePublication',
      'dateNoLongerInForce',
      'dateSessionStartedAtTime',
      'attachmentName',
      'hasAdditionalTaxRate',
      'taxRateAmount',
      'submission'
    ], reportData);
  }
};

