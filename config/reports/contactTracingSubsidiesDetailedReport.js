import {generateReportFromData, batchedQuery} from '../helpers.js';

export default {
  cronPattern: '0 55 23 * * *',
  name: 'contactTracingSubsidiesDetailedReport',
  execute: async () => {
    const reportData = {
      title: 'contactTracingSubsidiesDetailedReport',
      description: 'CSV detailed data dump of contact tracing subsidies',
      filePrefix: 'contactTracingSubsidiesDetailedReport'
    };
    console.log('Generate contactTracingSubsidiesDetailedReport');

    const queryStringPart1 = `
      PREFIX transactie: <http://data.vlaanderen.be/ns/transactie#>
      PREFIX m8g: <http://data.europa.eu/m8g/>
      PREFIX dct: <http://purl.org/dc/terms/>
      PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
      PREFIX adms: <http://www.w3.org/ns/adms#>

      SELECT DISTINCT ?smc ?bestuurseenheid ?aangemaaktOp ?bewerktOp ?gewijzigdDoor WHERE {
        ?smc transactie:isInstantieVan <http://lblod.data.gift/concepts/2697fbe1-4226-4325-807b-5dfa58e40a95> ;
          dct:created ?aangemaaktOp ;
          dct:modified ?bewerktOp ;
          ext:lastModifiedBy ?lastModifiedBy ;
          m8g:hasParticipation ?participation ;
          dct:source ?applicationForm .
        ?applicationForm adms:status <http://lblod.data.gift/concepts/9bd8d86d-bb10-4456-a84e-91e9507c374c> .
        ?bestuur m8g:playsRole ?participation ;
          skos:prefLabel ?bestuurseenheid .
        ?lastModifiedBy foaf:familyName ?familyName ;
          foaf:firstName ?firstName .
        BIND(CONCAT(?firstName, " ", ?familyName) AS ?gewijzigdDoor)
      }
    `;

    const queryResponsePart1 = await batchedQuery(queryStringPart1);
    const dataPart1 = queryResponsePart1.results.bindings.reduce( (acc, row) => {
      acc[getSafeValue(row, 'smc')] = {
        smc: getSafeValue(row, 'smc'),
        bestuurseenheid: getSafeValue(row, 'bestuurseenheid'),
        aangemaaktOp: getSafeValue(row, 'aangemaaktOp'),
        bewerktOp: getSafeValue(row, 'bewerktOp'),
        gewijzigdDoor: getSafeValue(row, 'gewijzigdDoor')
      };
      return acc;
    }, {});

    const queryStringPart2 = `
      PREFIX transactie: <http://data.vlaanderen.be/ns/transactie#>
      PREFIX schema: <http://schema.org/>
      PREFIX dct: <http://purl.org/dc/terms/>
      PREFIX foaf: <http://xmlns.com/foaf/0.1/>
      PREFIX adms: <http://www.w3.org/ns/adms#>

      SELECT ?smc ?voornaamContactpersoon ?familienaamContactpersoon ?telefoonnummer ?mailadres
      WHERE {
        ?smc transactie:isInstantieVan <http://lblod.data.gift/concepts/2697fbe1-4226-4325-807b-5dfa58e40a95> ;
          dct:source ?applicationForm .
        ?applicationForm schema:contactPoint ?contactPoint ;
          adms:status <http://lblod.data.gift/concepts/9bd8d86d-bb10-4456-a84e-91e9507c374c> .
        ?contactPoint schema:email ?mailadres ;
          foaf:familyName ?familienaamContactpersoon ;
          foaf:firstName ?voornaamContactpersoon ;
          schema:telephone ?telefoonnummer .
      }
    `;

    const queryResponsePart2 = await batchedQuery(queryStringPart2);
    const dataPart2 = queryResponsePart2.results.bindings.reduce( (acc, row) => {
      let dataPart = {
        voornaamContactpersoon: getSafeValue(row, 'voornaamContactpersoon'),
        familienaamContactpersoon: getSafeValue(row, 'familienaamContactpersoon'),
        telefoonnummer: getSafeValue(row, 'telefoonnummer'),
        mailadres: getSafeValue(row, 'mailadres')
      };
      acc[getSafeValue(row, 'smc')] = Object.assign(dataPart, dataPart1[getSafeValue(row, 'smc')]);
      return acc;
    }, {});

    const queryStringPart3 = `
      PREFIX transactie: <http://data.vlaanderen.be/ns/transactie#>
      PREFIX schema: <http://schema.org/>
      PREFIX dct: <http://purl.org/dc/terms/>
      PREFIX lblodSubsidie: <http://lblod.data.gift/vocabularies/subsidie/>
      PREFIX adms: <http://www.w3.org/ns/adms#>

      SELECT ?smc ?rekeningnummer ?samenwerkingsverband (group_concat(?ledenSamenwerking;separator=";") as ?ledenSamenwerking)
      WHERE {
        ?smc transactie:isInstantieVan <http://lblod.data.gift/concepts/2697fbe1-4226-4325-807b-5dfa58e40a95> ;
          dct:source ?applicationForm .
        ?applicationForm schema:bankAccount/schema:identifier ?rekeningnummer ;
          lblodSubsidie:isCollaboration/skos:prefLabel ?samenwerkingsverband ;
          adms:status <http://lblod.data.gift/concepts/9bd8d86d-bb10-4456-a84e-91e9507c374c> .
        
        OPTIONAL { ?applicationForm lblodSubsidie:collaborator/skos:prefLabel ?ledenSamenwerking . }
      }
      GROUP BY ?smc ?rekeningnummer ?samenwerkingsverband
    `;

    const queryResponsePart3 = await batchedQuery(queryStringPart3);
    const dataPart3 = queryResponsePart3.results.bindings.reduce( (acc, row) => {
      let dataPart = {
        rekeningnummer: getSafeValue(row, 'rekeningnummer'),
        samenwerkingsverband: getSafeValue(row, 'samenwerkingsverband'),
        ledenSamenwerking: getSafeValue(row, 'ledenSamenwerking')
      };
      acc[getSafeValue(row, 'smc')] = Object.assign(dataPart, dataPart2[getSafeValue(row, 'smc')]);
      return acc;
    }, {});

    const queryStringPart4 = `
      PREFIX transactie: <http://data.vlaanderen.be/ns/transactie#>
      PREFIX schema: <http://schema.org/>
      PREFIX dct: <http://purl.org/dc/terms/>
      PREFIX lblodSubsidie: <http://lblod.data.gift/vocabularies/subsidie/>
      PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
      PREFIX adms: <http://www.w3.org/ns/adms#>

      SELECT distinct ?smc ?november ?december ?januari ?februari ?maart ?april ?mei ?juni ?juli
              ?augustus ?bestaandPersoneelskader ?extraAangeworven
              ?vrijwilligers ?specifiekeUitgaven ?anderstaligen ?65plussers ?personenMetEenHandicap
              ?ondernemers ?personenMetBeperkteMobiliteit ?werklozen ?daklozen ?woonwagenbewoners
              ?personenMetEenLaagInkomen ?jongeren ?telefonischeContacten ?huisAanHuisBezoekenContact
              ?burgerloket ?email ?focusedPopulationAndereOption ?typesOfContactsAndereOption
      WHERE {
        ?smc transactie:isInstantieVan <http://lblod.data.gift/concepts/2697fbe1-4226-4325-807b-5dfa58e40a95> ;
          dct:source ?applicationForm .
        ?applicationForm lblodSubsidie:engagementTable/ext:engagementEntry ?entry ;
          adms:status <http://lblod.data.gift/concepts/9bd8d86d-bb10-4456-a84e-91e9507c374c> .
        ?entry ext:existingStaff ?bestaandPersoneelskader ;
          ext:additionalStaff ?extraAangeworven ;
          ext:volunteers ?vrijwilligers .
        
        OPTIONAL { ?applicationForm lblodSubsidie:extraContactTracingMeasuresNovember/skos:prefLabel ?november . }
        OPTIONAL { ?applicationForm lblodSubsidie:extraContactTracingMeasuresDecember/skos:prefLabel ?december . }
        OPTIONAL { ?applicationForm lblodSubsidie:extraContactTracingMeasuresJanuary/skos:prefLabel ?januari . }
        OPTIONAL { ?applicationForm lblodSubsidie:extraContactTracingMeasuresFebruary/skos:prefLabel ?februari . }
        OPTIONAL { ?applicationForm lblodSubsidie:extraContactTracingMeasuresMarch/skos:prefLabel ?maart . }
        OPTIONAL { ?applicationForm lblodSubsidie:extraContactTracingMeasuresApril/skos:prefLabel ?april . }
        OPTIONAL { ?applicationForm lblodSubsidie:extraContactTracingMeasuresMay/skos:prefLabel ?mei . }
        OPTIONAL { ?applicationForm lblodSubsidie:extraContactTracingMeasuresJune/skos:prefLabel ?juni . }
        OPTIONAL { ?applicationForm lblodSubsidie:extraContactTracingMeasuresJuly/skos:prefLabel ?juli . }
        OPTIONAL { ?applicationForm lblodSubsidie:extraContactTracingMeasuresAugust/skos:prefLabel ?augustus . }

        OPTIONAL { ?applicationForm lblodSubsidie:estimatedExtraCosts ?specifiekeUitgaven . }

        OPTIONAL { ?applicationForm lblodSubsidie:collaborator/skos:prefLabel ?ledenSamenwerking . }

        OPTIONAL {
          ?applicationForm lblodSubsidie:focusedPopulation ?anderstaligenRule .
          VALUES ?anderstaligenRule { <http://data.lblod.info/id/subsidies/rules/4ca04e45-cfbd-44e5-a91b-d070caff6b49> }
        }
        BIND(IF(BOUND(?anderstaligenRule), "ja", "nee") AS ?anderstaligen)

        OPTIONAL {
          ?applicationForm lblodSubsidie:focusedPopulation ?65plussersRule .
          VALUES ?65plussersRule { <http://data.lblod.info/id/subsidies/rules/d71b5fc9-c304-4687-b789-76a84296932f> }
        }
        BIND(IF(BOUND(?65plussersRule), "ja", "nee") AS ?65plussers)

        OPTIONAL {
          ?applicationForm lblodSubsidie:focusedPopulation ?personenMetEenHandicapRule .
          VALUES ?personenMetEenHandicapRule { <http://data.lblod.info/id/subsidies/rules/9f483029-cdc4-43f2-b07b-c6d3d2bd32ec> }
        }
        BIND(IF(BOUND(?personenMetEenHandicapRule), "ja", "nee") AS ?personenMetEenHandicap)

        OPTIONAL {
          ?applicationForm lblodSubsidie:focusedPopulation ?ondernemersRule .
          VALUES ?ondernemersRule { <http://data.lblod.info/id/subsidies/rules/384b2567-ab54-4b6f-b19c-a438829b3666> }
        }
        BIND(IF(BOUND(?ondernemersRule), "ja", "nee") AS ?ondernemers)

        OPTIONAL {
          ?applicationForm lblodSubsidie:focusedPopulation ?personenMetBeperkteMobiliteitRule .
          VALUES ?personenMetBeperkteMobiliteitRule { <http://data.lblod.info/id/subsidies/rules/34bac9ee-29fd-4fa9-b4f3-aaacf2c14a20> }
        }
        BIND(IF(BOUND(?personenMetBeperkteMobiliteitRule), "ja", "nee") AS ?personenMetBeperkteMobiliteit)

        OPTIONAL {
          ?applicationForm lblodSubsidie:focusedPopulation ?werklozenRule .
          VALUES ?werklozenRule { <http://data.lblod.info/id/subsidies/rules/baef0751-81b2-4b0c-88c9-4cdd635e0a21> }
        }
        BIND(IF(BOUND(?werklozenRule), "ja", "nee") AS ?werklozen)

        OPTIONAL {
          ?applicationForm lblodSubsidie:focusedPopulation ?daklozenRule .
          VALUES ?daklozenRule { <http://data.lblod.info/id/subsidies/rules/49f168af-7cc2-412b-afb2-ec9a0fd9760d> }
        }
        BIND(IF(BOUND(?daklozenRule), "ja", "nee") AS ?daklozen)

        OPTIONAL {
          ?applicationForm lblodSubsidie:focusedPopulation ?woonwagenbewonersRule .
          VALUES ?woonwagenbewonersRule { <http://data.lblod.info/id/subsidies/rules/a10ccc14-6b46-4fd6-ab9b-c6e45c673287> }
        }
        BIND(IF(BOUND(?woonwagenbewonersRule), "ja", "nee") AS ?woonwagenbewoners)

        OPTIONAL {
          ?applicationForm lblodSubsidie:focusedPopulation ?personenMetEenLaagInkomenRule .
          VALUES ?personenMetEenLaagInkomenRule { <http://data.lblod.info/id/subsidies/rules/34eb316a-488b-4ec3-a29e-450b016a85b7> }
        }
        BIND(IF(BOUND(?personenMetEenLaagInkomenRule), "ja", "nee") AS ?personenMetEenLaagInkomen)

        OPTIONAL {
          ?applicationForm lblodSubsidie:focusedPopulation ?jongerenRule .
          VALUES ?jongerenRule { <http://data.lblod.info/id/subsidies/rules/e9a7abaf-7098-4489-8fcb-aa73e80f0733> }
        }
        BIND(IF(BOUND(?jongerenRule), "ja", "nee") AS ?jongeren)

        OPTIONAL { ?applicationForm lblodSubsidie:focusedPopulationAndereOption ?focusedPopulationAndereOption . }

        OPTIONAL {
          ?applicationForm lblodSubsidie:typesOfContacts ?telefonischeContactenRule .
          VALUES ?telefonischeContactenRule { <http://data.lblod.info/id/subsidies/rules/94ee5c3c-2794-4fd2-9779-524f71e2005b> }
        }
        BIND(IF(BOUND(?telefonischeContactenRule), "ja", "nee") AS ?telefonischeContacten)

        OPTIONAL {
          ?applicationForm lblodSubsidie:typesOfContacts ?huisAanHuisBezoekenContactRule .
          VALUES ?huisAanHuisBezoekenContactRule { <http://data.lblod.info/id/subsidies/rules/17eda7bf-ef96-4871-bc65-f7fd96a4d937> }
        }
        BIND(IF(BOUND(?huisAanHuisBezoekenContactRule), "ja", "nee") AS ?huisAanHuisBezoekenContact)

        OPTIONAL {
          ?applicationForm lblodSubsidie:typesOfContacts ?burgerloketRule .
          VALUES ?burgerloketRule { <http://data.lblod.info/id/subsidies/rules/4899326e-096a-4a77-84d4-9af9ad70ab30> }
        }
        BIND(IF(BOUND(?burgerloketRule), "ja", "nee") AS ?burgerloket)

        OPTIONAL {
          ?applicationForm lblodSubsidie:typesOfContacts ?emailRule .
          VALUES ?emailRule { <http://data.lblod.info/id/subsidies/rules/fb34b0bf-2dfa-4ac1-a091-fd8f191d8197> }
        }
        BIND(IF(BOUND(?emailRule), "ja", "nee") AS ?email)

        OPTIONAL { ?applicationForm lblodSubsidie:typesOfContactsAndereOption ?typesOfContactsAndereOption . }
      }
    `;

    const queryResponsePart4 = await batchedQuery(queryStringPart4);
    const dataPart4 = queryResponsePart4.results.bindings.reduce( (acc, row) => {
      let dataPart = {
        november: getSafeValue(row, 'november'),
        december: getSafeValue(row, 'december'),
        januari: getSafeValue(row, 'januari'),
        februari: getSafeValue(row, 'februari'),
        maart: getSafeValue(row, 'maart'),
        april: getSafeValue(row, 'april'),
        mei: getSafeValue(row, 'mei'),
        juni: getSafeValue(row, 'juni'),
        juli: getSafeValue(row, 'juli'),
        augustus: getSafeValue(row, 'augustus'),
        bestaandPersoneelskader: getSafeValue(row, 'bestaandPersoneelskader'),
        extraAangeworven: getSafeValue(row, 'extraAangeworven'),
        vrijwilligers: getSafeValue(row, 'vrijwilligers'),
        specifiekeUitgaven: getSafeValue(row, 'specifiekeUitgaven'),
        anderstaligen: getSafeValue(row, 'anderstaligen'),
        vijfenzestigPlussers: getSafeValue(row, '65plussers'),
        personenMetEenHandicap: getSafeValue(row, 'personenMetEenHandicap'),
        ondernemers: getSafeValue(row, 'ondernemers'),
        personenMetBeperkteMobiliteit: getSafeValue(row, 'personenMetBeperkteMobiliteit'),
        werklozen: getSafeValue(row, 'werklozen'),
        daklozen: getSafeValue(row, 'daklozen'),
        woonwagenbewoners: getSafeValue(row, 'woonwagenbewoners'),
        personenMetEenLaagInkomen: getSafeValue(row, 'personenMetEenLaagInkomen'),
        jongeren: getSafeValue(row, 'jongeren'),
        telefonischeContacten: getSafeValue(row, 'telefonischeContacten'),
        huisAanHuisBezoekenContact: getSafeValue(row, 'huisAanHuisBezoekenContact'),
        burgerloket: getSafeValue(row, 'burgerloket'),
        email: getSafeValue(row, 'email'),
        focusedPopulationAndereOption: getSafeValue(row, 'focusedPopulationAndereOption'),
        typesOfContactsAndereOption: getSafeValue(row, 'typesOfContactsAndereOption')
      };
      acc[getSafeValue(row, 'smc')] = Object.assign(dataPart, dataPart3[getSafeValue(row, 'smc')]);
      return acc;
    }, {});

    const queryStringPart5 = `
      PREFIX transactie: <http://data.vlaanderen.be/ns/transactie#>
      PREFIX dct: <http://purl.org/dc/terms/>
      PREFIX lblodSubsidie: <http://lblod.data.gift/vocabularies/subsidie/>
      PREFIX adms: <http://www.w3.org/ns/adms#>

      SELECT ?smc ?prioEersteDoelstelling ?verspreidingVanFolders ?viaWebsite ?telefonischeContacten
              ?huisAanHuisBezoeken ?contactenViaMutualiteiten ?viaInfluencers ?viaArmoedeorganisaties
              ?viaDeVoedselbank ?preventionMethodsAndereOption
      WHERE {
        ?smc transactie:isInstantieVan <http://lblod.data.gift/concepts/2697fbe1-4226-4325-807b-5dfa58e40a95> ;
          dct:source ?applicationForm .
        ?applicationForm lblodSubsidie:contactAndSourceTrackingObjectiveOne/lblodSubsidie:priority/skos:prefLabel ?prioEersteDoelstelling ;
          adms:status <http://lblod.data.gift/concepts/9bd8d86d-bb10-4456-a84e-91e9507c374c> .
        
        OPTIONAL {
          ?applicationForm lblodSubsidie:preventionMethods ?verspreidingVanFoldersRule .
          VALUES ?verspreidingVanFoldersRule { <http://data.lblod.info/id/subsidies/rules/04a7e1c6-407a-4154-819e-de1cfa4dee92> }
        }
        BIND(IF(BOUND(?verspreidingVanFoldersRule), "ja", "nee") AS ?verspreidingVanFolders)
        
        OPTIONAL {
          ?applicationForm lblodSubsidie:preventionMethods ?viaWebsiteRule .
          VALUES ?viaWebsiteRule { <http://data.lblod.info/id/subsidies/rules/dd80f93f-e013-4cbb-840d-53b3c3a888fb> }
        }
        BIND(IF(BOUND(?viaWebsiteRule), "ja", "nee") AS ?viaWebsite)
        
        OPTIONAL {
          ?applicationForm lblodSubsidie:preventionMethods ?telefonischeContactenRule .
          VALUES ?telefonischeContactenRule { <http://data.lblod.info/id/subsidies/rules/1afe28e2-e6b7-4af3-86b6-1254e8ae7d87> }
        }
        BIND(IF(BOUND(?telefonischeContactenRule), "ja", "nee") AS ?telefonischeContacten)
        
        OPTIONAL {
          ?applicationForm lblodSubsidie:preventionMethods ?huisAanHuisBezoekenRule .
          VALUES ?huisAanHuisBezoekenRule { <http://data.lblod.info/id/subsidies/rules/f5d4ce8c-6baf-4304-8037-0aa7afe631e5> }
        }
        BIND(IF(BOUND(?huisAanHuisBezoekenRule), "ja", "nee") AS ?huisAanHuisBezoeken)
        
        OPTIONAL {
          ?applicationForm lblodSubsidie:preventionMethods ?contactenViaMutualiteitenRule .
          VALUES ?contactenViaMutualiteitenRule { <http://data.lblod.info/id/subsidies/rules/aa995916-6deb-478b-b332-9300f494044d> }
        }
        BIND(IF(BOUND(?contactenViaMutualiteitenRule), "ja", "nee") AS ?contactenViaMutualiteiten)
        
        OPTIONAL {
          ?applicationForm lblodSubsidie:preventionMethods ?viaInfluencersRule .
          VALUES ?viaInfluencersRule { <http://data.lblod.info/id/subsidies/rules/e2ab52dc-7b22-40a7-9fa4-51973c3bad5a> }
        }
        BIND(IF(BOUND(?viaInfluencersRule), "ja", "nee") AS ?viaInfluencers)
        
        OPTIONAL {
          ?applicationForm lblodSubsidie:preventionMethods ?viaArmoedeorganisatiesRule .
          VALUES ?viaArmoedeorganisatiesRule { <http://data.lblod.info/id/subsidies/rules/d0c60eba-dbd0-46a5-a237-7d26f465af28> }
        }
        BIND(IF(BOUND(?viaArmoedeorganisatiesRule), "ja", "nee") AS ?viaArmoedeorganisaties)
        
        OPTIONAL {
          ?applicationForm lblodSubsidie:preventionMethods ?viaDeVoedselbankRule .
          VALUES ?viaDeVoedselbankRule { <http://data.lblod.info/id/subsidies/rules/dee57634-9b26-454d-bb33-f32f4d1c9bf6> }
        }
        BIND(IF(BOUND(?viaDeVoedselbankRule), "ja", "nee") AS ?viaDeVoedselbank)

        OPTIONAL { ?applicationForm lblodSubsidie:preventionMethodsAndereOption ?preventionMethodsAndereOption . }
      }
    `;

    const queryResponsePart5 = await batchedQuery(queryStringPart5);
    const dataPart5 = queryResponsePart5.results.bindings.reduce( (acc, row) => {
      let dataPart = {
        prioEersteDoelstelling: getSafeValue(row, 'prioEersteDoelstelling'),
        verspreidingVanFolders: getSafeValue(row, 'verspreidingVanFolders'),
        viaWebsite: getSafeValue(row, 'viaWebsite'),
        telefonischeContacten: getSafeValue(row, 'telefonischeContacten'),
        huisAanHuisBezoeken: getSafeValue(row, 'huisAanHuisBezoeken'),
        contactenViaMutualiteiten: getSafeValue(row, 'contactenViaMutualiteiten'),
        viaInfluencers: getSafeValue(row, 'viaInfluencers'),
        viaArmoedeorganisaties: getSafeValue(row, 'viaArmoedeorganisaties'),
        viaDeVoedselbank: getSafeValue(row, 'viaDeVoedselbank'),
        preventionMethodsAndereOption: getSafeValue(row, 'preventionMethodsAndereOption'),
      };
      acc[getSafeValue(row, 'smc')] = Object.assign(dataPart, dataPart4[getSafeValue(row, 'smc')]);
      return acc;
    }, {});

    const queryStringPart6 = `
      PREFIX transactie: <http://data.vlaanderen.be/ns/transactie#>
      PREFIX dct: <http://purl.org/dc/terms/>
      PREFIX lblodSubsidie: <http://lblod.data.gift/vocabularies/subsidie/>
      PREFIX adms: <http://www.w3.org/ns/adms#>

      SELECT ?smc ?prioTweedeDoelstelling ?contacterenVanBesmettePersonen ?meldenVanClusters ?monitorenVanEvenementen
              ?verzamelenVanHorecalijsten ?doorgevenVanContacten ?overlegMetDeZorgraad ?isolerenOfTijdelijkeSluitingVanClusters
              ?sourceDetectionActionsAndereOption
      WHERE {
        ?smc transactie:isInstantieVan <http://lblod.data.gift/concepts/2697fbe1-4226-4325-807b-5dfa58e40a95> ;
          dct:source ?applicationForm .
        ?applicationForm lblodSubsidie:contactAndSourceTrackingObjectiveTwo/lblodSubsidie:priority/skos:prefLabel ?prioTweedeDoelstelling ;
          adms:status <http://lblod.data.gift/concepts/9bd8d86d-bb10-4456-a84e-91e9507c374c> .
        
        OPTIONAL {
          ?applicationForm lblodSubsidie:sourceDetectionActions ?contacterenVanBesmettePersonenRule .
          VALUES ?contacterenVanBesmettePersonenRule { <http://data.lblod.info/id/subsidies/rules/e8fe4796-64be-4995-82ce-ba9f355fb5b4> }
        }
        BIND(IF(BOUND(?contacterenVanBesmettePersonenRule), "ja", "nee") AS ?contacterenVanBesmettePersonen)
        
        OPTIONAL {
          ?applicationForm lblodSubsidie:sourceDetectionActions ?meldenVanClustersRule .
          VALUES ?meldenVanClustersRule { <http://data.lblod.info/id/subsidies/rules/acd2e32c-fdcd-4ff5-991b-b0399296ec24> }
        }
        BIND(IF(BOUND(?meldenVanClustersRule), "ja", "nee") AS ?meldenVanClusters)
        
        OPTIONAL {
          ?applicationForm lblodSubsidie:sourceDetectionActions ?monitorenVanEvenementenRule .
          VALUES ?monitorenVanEvenementenRule { <http://data.lblod.info/id/subsidies/rules/9bd53c83-f67e-48a4-8b65-051e018cc3bf> }
        }
        BIND(IF(BOUND(?monitorenVanEvenementenRule), "ja", "nee") AS ?monitorenVanEvenementen)
        
        OPTIONAL {
          ?applicationForm lblodSubsidie:sourceDetectionActions ?verzamelenVanHorecalijstenRule .
          VALUES ?verzamelenVanHorecalijstenRule { <http://data.lblod.info/id/subsidies/rules/06720e8f-f786-43cc-8d4a-3355505f7278> }
        }
        BIND(IF(BOUND(?verzamelenVanHorecalijstenRule), "ja", "nee") AS ?verzamelenVanHorecalijsten)
        
        OPTIONAL {
          ?applicationForm lblodSubsidie:sourceDetectionActions ?doorgevenVanContactenRule .
          VALUES ?doorgevenVanContactenRule { <http://data.lblod.info/id/subsidies/rules/a13b8675-10fb-44d4-bdb0-e7bf141e96e5> }
        }
        BIND(IF(BOUND(?doorgevenVanContactenRule), "ja", "nee") AS ?doorgevenVanContacten)
        
        OPTIONAL {
          ?applicationForm lblodSubsidie:sourceDetectionActions ?overlegMetDeZorgraadRule .
          VALUES ?overlegMetDeZorgraadRule { <http://data.lblod.info/id/subsidies/rules/0bef5e29-c866-427c-88f9-187331b4bf3c> }
        }
        BIND(IF(BOUND(?overlegMetDeZorgraadRule), "ja", "nee") AS ?overlegMetDeZorgraad)
        
        OPTIONAL {
          ?applicationForm lblodSubsidie:sourceDetectionActions ?isolerenOfTijdelijkeSluitingVanClustersRule .
          VALUES ?isolerenOfTijdelijkeSluitingVanClustersRule { <http://data.lblod.info/id/subsidies/rules/d3dd104d-dac1-422e-9ad5-d9dabbcb2bbe> }
        }
        BIND(IF(BOUND(?isolerenOfTijdelijkeSluitingVanClustersRule), "ja", "nee") AS ?isolerenOfTijdelijkeSluitingVanClusters)

        OPTIONAL { ?applicationForm lblodSubsidie:sourceDetectionActionsAndereOption ?sourceDetectionActionsAndereOption . }
      }
    `;

    const queryResponsePart6 = await batchedQuery(queryStringPart6);
    const dataPart6 = queryResponsePart6.results.bindings.reduce( (acc, row) => {
      let dataPart = {
        prioTweedeDoelstelling: getSafeValue(row, 'prioTweedeDoelstelling'),
        contacterenVanBesmettePersonen: getSafeValue(row, 'contacterenVanBesmettePersonen'),
        meldenVanClusters: getSafeValue(row, 'meldenVanClusters'),
        monitorenVanEvenementen: getSafeValue(row, 'monitorenVanEvenementen'),
        verzamelenVanHorecalijsten: getSafeValue(row, 'verzamelenVanHorecalijsten'),
        doorgevenVanContacten: getSafeValue(row, 'doorgevenVanContacten'),
        overlegMetDeZorgraad: getSafeValue(row, 'overlegMetDeZorgraad'),
        isolerenOfTijdelijkeSluitingVanClusters: getSafeValue(row, 'isolerenOfTijdelijkeSluitingVanClusters'),
        sourceDetectionActionsAndereOption: getSafeValue(row, 'sourceDetectionActionsAndereOption')
      };
      acc[getSafeValue(row, 'smc')] = Object.assign(dataPart, dataPart5[getSafeValue(row, 'smc')]);
      return acc;
    }, {});

    const queryStringPart7 = `
      PREFIX transactie: <http://data.vlaanderen.be/ns/transactie#>
      PREFIX dct: <http://purl.org/dc/terms/>
      PREFIX lblodSubsidie: <http://lblod.data.gift/vocabularies/subsidie/>
      PREFIX adms: <http://www.w3.org/ns/adms#>

      SELECT ?smc ?prioDerdeDoelstelling ?tijdelijkeWoongelegenheidTerBeschikkingGesteld ?quarantaineEenCentrumVoorHerstelverblijf
              ?huisbezoekDoorQuarantainecoach ?belrondeDoorQuarantainecoach ?uitleggenVanMaatregelen
              ?praktischeHulp ?doorverwijzingNaarGespecialiseerdeHulp ?financieleHulpvragenDoorgevenNaarDeJuisteInstanties
              ?quarantaineCoachingMethodsAndereOption
      WHERE {
        ?smc transactie:isInstantieVan <http://lblod.data.gift/concepts/2697fbe1-4226-4325-807b-5dfa58e40a95> ;
          dct:source ?applicationForm .
        ?applicationForm lblodSubsidie:contactAndSourceTrackingObjectiveThree/lblodSubsidie:priority/skos:prefLabel ?prioDerdeDoelstelling ;
          adms:status <http://lblod.data.gift/concepts/9bd8d86d-bb10-4456-a84e-91e9507c374c> .

        OPTIONAL {
          ?applicationForm lblodSubsidie:quarantaineCoachingMethods ?tijdelijkeWoongelegenheidTerBeschikkingGesteldRule .
          VALUES ?tijdelijkeWoongelegenheidTerBeschikkingGesteldRule { <http://data.lblod.info/id/subsidies/rules/71ea6694-81df-43fb-a125-a328e71d22e2> }
        }
        BIND(IF(BOUND(?tijdelijkeWoongelegenheidTerBeschikkingGesteldRule), "ja", "nee") AS ?tijdelijkeWoongelegenheidTerBeschikkingGesteld)

        OPTIONAL {
          ?applicationForm lblodSubsidie:quarantaineCoachingMethods ?quarantaineEenCentrumVoorHerstelverblijfRule .
          VALUES ?quarantaineEenCentrumVoorHerstelverblijfRule { <http://data.lblod.info/id/subsidies/rules/9b6be38f-ea5d-4978-9a4b-59e8e235e772> }
        }
        BIND(IF(BOUND(?quarantaineEenCentrumVoorHerstelverblijfRule), "ja", "nee") AS ?quarantaineEenCentrumVoorHerstelverblijf)

        OPTIONAL {
          ?applicationForm lblodSubsidie:quarantaineCoachingMethods ?huisbezoekDoorQuarantainecoachRule .
          VALUES ?huisbezoekDoorQuarantainecoachRule { <http://data.lblod.info/id/subsidies/rules/7a1e123f-2f7e-44d0-a107-654eb1aa8746> }
        }
        BIND(IF(BOUND(?huisbezoekDoorQuarantainecoachRule), "ja", "nee") AS ?huisbezoekDoorQuarantainecoach)

        OPTIONAL {
          ?applicationForm lblodSubsidie:quarantaineCoachingMethods ?belrondeDoorQuarantainecoachRule .
          VALUES ?belrondeDoorQuarantainecoachRule { <http://data.lblod.info/id/subsidies/rules/c9190e70-2f94-4edb-8ea5-bcfbaf4a9cfc> }
        }
        BIND(IF(BOUND(?belrondeDoorQuarantainecoachRule), "ja", "nee") AS ?belrondeDoorQuarantainecoach)

        OPTIONAL {
          ?applicationForm lblodSubsidie:quarantaineCoachingMethods ?uitleggenVanMaatregelenRule .
          VALUES ?uitleggenVanMaatregelenRule { <http://data.lblod.info/id/subsidies/rules/60306cda-a2d2-40ea-99d2-0ec1bfa0805d> }
        }
        BIND(IF(BOUND(?uitleggenVanMaatregelenRule), "ja", "nee") AS ?uitleggenVanMaatregelen)

        OPTIONAL {
          ?applicationForm lblodSubsidie:quarantaineCoachingMethods ?praktischeHulpRule .
          VALUES ?praktischeHulpRule { <http://data.lblod.info/id/subsidies/rules/9dbc9843-7ac4-47c5-a381-311735068ebf> }
        }
        BIND(IF(BOUND(?praktischeHulpRule), "ja", "nee") AS ?praktischeHulp)

        OPTIONAL {
          ?applicationForm lblodSubsidie:quarantaineCoachingMethods ?doorverwijzingNaarGespecialiseerdeHulpRule .
          VALUES ?doorverwijzingNaarGespecialiseerdeHulpRule { <http://data.lblod.info/id/subsidies/rules/182df43f-10af-4be5-875e-1a0e8051d559> }
        }
        BIND(IF(BOUND(?doorverwijzingNaarGespecialiseerdeHulpRule), "ja", "nee") AS ?doorverwijzingNaarGespecialiseerdeHulp)

        OPTIONAL {
          ?applicationForm lblodSubsidie:quarantaineCoachingMethods ?financieleHulpvragenDoorgevenNaarDeJuisteInstantiesRule .
          VALUES ?financieleHulpvragenDoorgevenNaarDeJuisteInstantiesRule { <http://data.lblod.info/id/subsidies/rules/92af1821-29f6-4b73-b9b5-95f1bf92fe3d> }
        }
        BIND(IF(BOUND(?financieleHulpvragenDoorgevenNaarDeJuisteInstantiesRule), "ja", "nee") AS ?financieleHulpvragenDoorgevenNaarDeJuisteInstanties)

        OPTIONAL { ?applicationForm lblodSubsidie:quarantaineCoachingMethodsAndereOption ?quarantaineCoachingMethodsAndereOption . }
      }
    `;

    const queryResponsePart7 = await batchedQuery(queryStringPart7);
    const dataPart7 = queryResponsePart7.results.bindings.reduce( (acc, row) => {
      let dataPart = {
        prioDerdeDoelstelling: getSafeValue(row, 'prioDerdeDoelstelling'),
        tijdelijkeWoongelegenheidTerBeschikkingGesteld: getSafeValue(row, 'tijdelijkeWoongelegenheidTerBeschikkingGesteld'),
        quarantaineEenCentrumVoorHerstelverblijf: getSafeValue(row, 'quarantaineEenCentrumVoorHerstelverblijf'),
        huisbezoekDoorQuarantainecoach: getSafeValue(row, 'huisbezoekDoorQuarantainecoach'),
        belrondeDoorQuarantainecoach: getSafeValue(row, 'belrondeDoorQuarantainecoach'),
        uitleggenVanMaatregelen: getSafeValue(row, 'uitleggenVanMaatregelen'),
        praktischeHulp: getSafeValue(row, 'praktischeHulp'),
        doorverwijzingNaarGespecialiseerdeHulp: getSafeValue(row, 'doorverwijzingNaarGespecialiseerdeHulp'),
        financieleHulpvragenDoorgevenNaarDeJuisteInstanties: getSafeValue(row, 'financieleHulpvragenDoorgevenNaarDeJuisteInstanties'),
        quarantaineCoachingMethodsAndereOption: getSafeValue(row, 'quarantaineCoachingMethodsAndereOption')
      };
      acc[getSafeValue(row, 'smc')] = Object.assign(dataPart, dataPart6[getSafeValue(row, 'smc')]);
      return acc;
    }, {});

    const queryStringPart8 = `
      PREFIX transactie: <http://data.vlaanderen.be/ns/transactie#>
      PREFIX dct: <http://purl.org/dc/terms/>
      PREFIX lblodSubsidie: <http://lblod.data.gift/vocabularies/subsidie/>
      PREFIX adms: <http://www.w3.org/ns/adms#>

      SELECT ?smc ?prioVierdeDoelstelling ?boodschappenGedaanVoorDeGezinnen ?samenaankoopactieGeorganiseerd
              ?informatieOpMaat ?coachingMetTolk ?huisbezoek ?psychologischeOfMentaleOndersteuning
              ?financieleOndersteuning ?vulnerableGroupsHelpAndereOption
      WHERE {
        ?smc transactie:isInstantieVan <http://lblod.data.gift/concepts/2697fbe1-4226-4325-807b-5dfa58e40a95> ;
          dct:source ?applicationForm .
        ?applicationForm lblodSubsidie:contactAndSourceTrackingObjectiveFour/lblodSubsidie:priority/skos:prefLabel ?prioVierdeDoelstelling ;
          adms:status <http://lblod.data.gift/concepts/9bd8d86d-bb10-4456-a84e-91e9507c374c> .

        OPTIONAL {
          ?applicationForm lblodSubsidie:vulnerableGroupsHelp ?boodschappenGedaanVoorDeGezinnenRule .
          VALUES ?boodschappenGedaanVoorDeGezinnenRule { <http://data.lblod.info/id/subsidies/rules/2ddf912c-e71a-45ae-847b-f9e94ab90d70> }
        }
        BIND(IF(BOUND(?boodschappenGedaanVoorDeGezinnenRule), "ja", "nee") AS ?boodschappenGedaanVoorDeGezinnen)

        OPTIONAL {
          ?applicationForm lblodSubsidie:vulnerableGroupsHelp ?samenaankoopactieGeorganiseerdRule .
          VALUES ?samenaankoopactieGeorganiseerdRule { <http://data.lblod.info/id/subsidies/rules/c34db3c6-157c-4115-86dd-de0cb953a240> }
        }
        BIND(IF(BOUND(?samenaankoopactieGeorganiseerdRule), "ja", "nee") AS ?samenaankoopactieGeorganiseerd)

        OPTIONAL {
          ?applicationForm lblodSubsidie:vulnerableGroupsHelp ?informatieOpMaatRule .
          VALUES ?informatieOpMaatRule { <http://data.lblod.info/id/subsidies/rules/1bdfcee0-339e-4c94-8b76-386481b142cb> }
        }
        BIND(IF(BOUND(?informatieOpMaatRule), "ja", "nee") AS ?informatieOpMaat)

        OPTIONAL {
          ?applicationForm lblodSubsidie:vulnerableGroupsHelp ?coachingMetTolkRule .
          VALUES ?coachingMetTolkRule { <http://data.lblod.info/id/subsidies/rules/4e035c5b-195f-42ff-b9b7-9f356949370d> }
        }
        BIND(IF(BOUND(?coachingMetTolkRule), "ja", "nee") AS ?coachingMetTolk)

        OPTIONAL {
          ?applicationForm lblodSubsidie:vulnerableGroupsHelp ?huisbezoekRule .
          VALUES ?huisbezoekRule { <http://data.lblod.info/id/subsidies/rules/5458ea7e-8692-459c-8208-fef02601a757> }
        }
        BIND(IF(BOUND(?huisbezoekRule), "ja", "nee") AS ?huisbezoek)

        OPTIONAL {
          ?applicationForm lblodSubsidie:vulnerableGroupsHelp ?psychologischeOfMentaleOndersteuningRule .
          VALUES ?psychologischeOfMentaleOndersteuningRule { <http://data.lblod.info/id/subsidies/rules/8a5c1696-7198-41af-8f41-d63f9a53f1a1> }
        }
        BIND(IF(BOUND(?psychologischeOfMentaleOndersteuningRule), "ja", "nee") AS ?psychologischeOfMentaleOndersteuning)

        OPTIONAL {
          ?applicationForm lblodSubsidie:vulnerableGroupsHelp ?financieleOndersteuningRule .
          VALUES ?financieleOndersteuningRule { <http://data.lblod.info/id/subsidies/rules/e7c07f5f-803f-4c0a-b5e1-d1ac03a82dac> }
        }
        BIND(IF(BOUND(?financieleOndersteuningRule), "ja", "nee") AS ?financieleOndersteuning)

        OPTIONAL { ?applicationForm lblodSubsidie:vulnerableGroupsHelpAndereOption ?vulnerableGroupsHelpAndereOption . }
      }
    `;

    const queryResponsePart8 = await batchedQuery(queryStringPart8);
    const dataPart8 = queryResponsePart8.results.bindings.reduce( (acc, row) => {
      let dataPart = {
        prioVierdeDoelstelling: getSafeValue(row, 'prioVierdeDoelstelling'),
        boodschappenGedaanVoorDeGezinnen: getSafeValue(row, 'boodschappenGedaanVoorDeGezinnen'),
        samenaankoopactieGeorganiseerd: getSafeValue(row, 'samenaankoopactieGeorganiseerd'),
        informatieOpMaat: getSafeValue(row, 'informatieOpMaat'),
        coachingMetTolk: getSafeValue(row, 'coachingMetTolk'),
        huisbezoek: getSafeValue(row, 'huisbezoek'),
        psychologischeOfMentaleOndersteuning: getSafeValue(row, 'psychologischeOfMentaleOndersteuning'),
        financieleOndersteuning: getSafeValue(row, 'financieleOndersteuning'),
        vulnerableGroupsHelpAndereOption: getSafeValue(row, 'vulnerableGroupsHelpAndereOption'),
      };
      acc[getSafeValue(row, 'smc')] = Object.assign(dataPart, dataPart7[getSafeValue(row, 'smc')]);
      return acc;
    }, {});

    await generateReportFromData(Object.values(dataPart8), [
      'bestuurseenheid',
      'aangemaaktOp',
      'bewerktOp',
      'gewijzigdDoor',
      'voornaamContactpersoon',
      'familienaamContactpersoon',
      'telefoonnummer',
      'mailadres',
      'rekeningnummer',
      'samenwerkingsverband',
      'ledenSamenwerking',
      'november',
      'december',
      'januari',
      'februari',
      'maart',
      'april',
      'mei',
      'juni',
      'juli',
      'augustus',
      'bestaandPersoneelskader',
      'extraAangeworven',
      'vrijwilligers',
      'specifiekeUitgaven',
      'anderstaligen',
      'vijfenzestigPlussers',
      'personenMetEenHandicap',
      'ondernemers',
      'personenMetBeperkteMobiliteit',
      'werklozen',
      'daklozen',
      'woonwagenbewoners',
      'personenMetEenLaagInkomen',
      'jongeren',
      'focusedPopulationAndereOption',
      'telefonischeContacten',
      'huisAanHuisBezoekenContact',
      'burgerloket',
      'email',
      'typesOfContactsAndereOption',
      'prioEersteDoelstelling',
      'viaWebsite',
      'telefonischeContacten',
      'huisAanHuisBezoeken',
      'contactenViaMutualiteiten',
      'viaInfluencers',
      'viaArmoedeorganisaties',
      'viaDeVoedselbank',
      'preventionMethodsAndereOption',
      'prioTweedeDoelstelling',
      'contacterenVanBesmettePersonen',
      'meldenVanClusters',
      'monitorenVanEvenementen',
      'verzamelenVanHorecalijsten',
      'doorgevenVanContacten',
      'overlegMetDeZorgraad',
      'isolerenOfTijdelijkeSluitingVanClusters',
      'sourceDetectionActionsAndereOption',
      'prioDerdeDoelstelling',
      'tijdelijkeWoongelegenheidTerBeschikkingGesteld',
      'quarantaineEenCentrumVoorHerstelverblijf',
      'huisbezoekDoorQuarantainecoach',
      'belrondeDoorQuarantainecoach',
      'uitleggenVanMaatregelen',
      'praktischeHulp',
      'doorverwijzingNaarGespecialiseerdeHulp',
      'financieleHulpvragenDoorgevenNaarDeJuisteInstanties',
      'quarantaineCoachingMethodsAndereOption',
      'prioVierdeDoelstelling',
      'boodschappenGedaanVoorDeGezinnen',
      'samenaankoopactieGeorganiseerd',
      'informatieOpMaat',
      'coachingMetTolk',
      'huisbezoek',
      'psychologischeOfMentaleOndersteuning',
      'financieleOndersteuning',
      'vulnerableGroupsHelpAndereOption',
      'smc'
    ], reportData);
  }
};

function getSafeValue(entry, property) {
  return entry[property] ? wrapInQuote(entry[property].value) : null;
}

// Some values might contain comas, wrapping them in escapes quotes doesn't disturb the colomns
function wrapInQuote(value) {
  return `\"${value}\"`;
}
