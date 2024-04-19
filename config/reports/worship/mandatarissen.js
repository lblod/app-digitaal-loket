import * as mas from '@lblod/mu-auth-sudo';
import * as queries from './queries';
import * as hel from '../../helpers';
import * as n3 from 'n3';
import * as uti from './utils';
import { NAMESPACES as ns } from './namespaces';
import { SparqlJsonParser } from 'sparqljson-parse';
const sparqlJsonParser = new SparqlJsonParser();

// Note: it seems for this flow the batch-size is optimal in speed here,
// We suspect query parsing being the bottlneck in this scenario.
const BATCH_SIZE = 20;
const connectionOptions = {
  sparqlEndpoint: 'http://virtuoso:8890/sparql',
  mayRetry: true,
};

async function generate() {
  const mandatarissenResponse = await hel.batchedQuery(
    queries.allMandatarissen(),
    BATCH_SIZE
  );
  const mandatarissen = sparqlJsonParser
    .parseJsonResults(mandatarissenResponse)
    .map((m) => m.mandataris)
    .sort((a, b) => a.value < b.value);

  let allData = [];

  for (
    let mandatarisCounter = 0;
    mandatarisCounter < mandatarissen.length;
    mandatarisCounter += BATCH_SIZE
  ) {
    const mandatarissenSlice = mandatarissen.slice(
      mandatarisCounter,
      mandatarisCounter + BATCH_SIZE
    );

    //Personen
    const persoonQuery = queries.domainToRange(
      mandatarissenSlice,
      ns.mandaat`isBestuurlijkeAliasVan`,
      ns.person`Person`
    );
    const personenResponse = await mas.querySudo(
      persoonQuery,
      undefined,
      connectionOptions
    );
    const personen = sparqlJsonParser
      .parseJsonResults(personenResponse)
      .map((i) => i.range);

    //Identifiers
    const identifierQuery = queries.domainToRange(
      personen,
      ns.adms`identifier`,
      ns.adms`Identifier`
    );
    const identifierResponse = await mas.querySudo(
      identifierQuery,
      undefined,
      connectionOptions
    );
    const identifiers = sparqlJsonParser
      .parseJsonResults(identifierResponse)
      .map((i) => i.range);

    //Geboortes
    const geboorteQuery = queries.domainToRange(
      personen,
      ns.persoon`heeftGeboorte`,
      ns.persoon`Geboorte`
    );
    const geboortesResponse = await mas.querySudo(
      geboorteQuery,
      undefined,
      connectionOptions
    );
    const geboortes = sparqlJsonParser
      .parseJsonResults(geboortesResponse)
      .map((i) => i.range);

    //Contacts
    const contactQuery = queries.domainToRange(
      mandatarissenSlice,
      ns.sch`contactPoint`,
      ns.sch`ContactPoint`
    );
    const contactsResponse = await mas.querySudo(
      contactQuery,
      undefined,
      connectionOptions
    );
    const contacts = sparqlJsonParser
      .parseJsonResults(contactsResponse)
      .map((i) => i.range);

    //Adresses
    const adresQuery = queries.domainToRange(
      contacts,
      ns.locn`address`,
      ns.locn`Address`
    );
    const addressResponse = await mas.querySudo(
      adresQuery,
      undefined,
      connectionOptions
    );
    const addresses = sparqlJsonParser
      .parseJsonResults(addressResponse)
      .map((i) => i.range);

    //Positions
    const positionQuery = queries.domainToRange(
      mandatarissenSlice,
      ns.org`holds`,
      ns.mandaat`Mandaat`
    );
    const positionsResponse = await mas.querySudo(
      positionQuery,
      undefined,
      connectionOptions
    );
    const positions = sparqlJsonParser
      .parseJsonResults(positionsResponse)
      .map((i) => i.range);

    //Functions
    const functionQuery = queries.domainToRange(
      positions,
      ns.org`role`,
      ns.ext`BestuursfunctieCode`
    );
    const functionResponse = await mas.querySudo(
      functionQuery,
      undefined,
      connectionOptions
    );
    const functions = sparqlJsonParser
      .parseJsonResults(functionResponse)
      .map((i) => i.range);

    //Besturen
    const besturenInTijdQuery = queries.rangeToDomain(
      positions,
      ns.org`hasPost`,
      ns.besluit`Bestuursorgaan`
    );
    const besturenInTijdResponse = await mas.querySudo(
      besturenInTijdQuery,
      undefined,
      connectionOptions
    );
    const besturenInTijd = sparqlJsonParser
      .parseJsonResults(besturenInTijdResponse)
      .map((i) => i.range);

    const besturenQuery = queries.domainToRange(
      besturenInTijd,
      ns.mandaat`isTijdspecialisatieVan`,
      ns.besluit`Bestuursorgaan`
    );
    const besturenResponse = await mas.querySudo(
      besturenQuery,
      undefined,
      connectionOptions
    );
    const besturen = sparqlJsonParser
      .parseJsonResults(besturenResponse)
      .map((i) => i.range);

    //Eenheden
    const eenhedenQuery = queries.domainToRange(
      besturen,
      ns.besluit`bestuurt`,
      ns.besluit`Bestuurseenheid`
    );
    const eenhedenResponse = await mas.querySudo(
      eenhedenQuery,
      undefined,
      connectionOptions
    );
    const eenheden = sparqlJsonParser
      .parseJsonResults(eenhedenResponse)
      .map((i) => i.range);

    // //TypeEredienst
    const typeEredienstQuery = queries.domainToRange(
      eenheden,
      ns.ere`typeEredienst`,
      ns.organ`TypeEredienst`
    );
    const typeEredienstResponse = await mas.querySudo(
      typeEredienstQuery,
      undefined,
      connectionOptions
    );
    const typeEredienst = sparqlJsonParser
      .parseJsonResults(typeEredienstResponse)
      .map((i) => i.range);

    const allSubjects = uti.dedup(
      [
        ...mandatarissenSlice,
        ...personen,
        ...identifiers,
        ...geboortes,
        ...contacts,
        ...addresses,
        ...positions,
        ...functions,
        ...besturenInTijd,
        ...besturen,
        ...eenheden,
        ...typeEredienst,
      ],
      'value'
    );

    const tripleData = new n3.Store();

    for (let counter = 0; counter < allSubjects.length; counter += BATCH_SIZE) {
      const subjectSlice = allSubjects.slice(counter, counter + BATCH_SIZE);
      const dataQuery = queries.dataForSubjects(subjectSlice);
      const dataResponse = await mas.querySudo(
        dataQuery,
        undefined,
        connectionOptions
      );
      const dataParsed = sparqlJsonParser.parseJsonResults(dataResponse);
      dataParsed.forEach((e) => tripleData.addQuad(e.s, e.p, e.o));
    }

    const combinedData = combineMandatarissenData(tripleData);
    allData = allData.concat(combinedData);
  }

  await hel.generateReportFromData(
    allData,
    [
      'eenheid',
      'eenheidnaam',
      'typeEredienstLabel',
      'bestuurnaam',
      'bestuurInTijd',
      'bestuurInTijdStart',
      'bestuurInTijdEinde',
      'mandataris',
      'afkomstGegevens',
      'vendorUri',
      'rolnaam',
      'startDatum',
      'eindeDatum',
      'geplandEinde',
      'persoon',
      'familienaam',
      'voornaam',
      'geboorteDatum',
      'rrnummer',
      'nationaliteit',
      'geslacht',
      'contact',
      'contactSoort',
      'email',
      'telefoon',
      'adres',
      'straat',
      'huisnummer',
      'busnummer',
      'postcode',
      'stad',
      'land',
    ],
    {
      title: 'Eredienst Mandatarissen Report',
      description: 'All eredienst Mandatarissen and their information.',
      filePrefix: 'eredienst-mandatarissen',
    }
  );
}

function combineMandatarissenData(store) {
  const data = [];
  const mandatarissen = store.getSubjects(
    ns.rdf`type`,
    ns.ere`EredienstMandataris`
  );

  for (const mandataris of mandatarissen) {
    const afkomstGegevens = store
      .readQuads(mandataris, ns.prov`wasGeneratedBy`)
      .next().value?.object?.value;
    const vendorUri = store.readQuads(mandataris, ns.owl`sameAs`).next().value
      ?.object?.value;
    const startDatum = store.readQuads(mandataris, ns.mandaat`start`).next()
      .value?.object?.value;
    const eindeDatum = store.readQuads(mandataris, ns.mandaat`einde`).next()
      .value?.object?.value;
    const geplandEinde = store
      .readQuads(mandataris, ns.ere`geplandeEinddatumAanstelling`)
      .next().value?.object?.value;

    const collect = {
      mandataris: mandataris.value,
      afkomstGegevens,
      vendorUri,
      startDatum,
      eindeDatum,
      geplandEinde,
    };

    const persoon = store
      .getObjects(mandataris, ns.mandaat`isBestuurlijkeAliasVan`)
      .filter((p) => store.has(p, ns.rdf`type`, ns.person`Person`))[0];

    if (persoon) {
      const familienaam = store.readQuads(persoon, ns.foaf`familyName`).next()
        .value?.object?.value;
      const voornaam = store
        .readQuads(persoon, ns.persoon`gebruikteVoornaam`)
        .next().value?.object?.value;
      const nationaliteit = store
        .readQuads(persoon, ns.persoon`heeftNationaliteit`)
        .next().value?.object?.value;
      const geslacht = store.readQuads(persoon, ns.persoon`geslacht`).next()
        .value?.object?.value;
      collect.persoon = persoon.value;
      collect.familienaam = familienaam;
      collect.voornaam = voornaam;
      collect.nationaliteit = nationaliteit;
      collect.geslacht = geslacht;

      const geboorteDatum = store
        .getObjects(persoon, ns.persoon`heeftGeboorte`)
        .filter((p) => store.has(p, ns.rdf`type`, ns.persoon`Geboorte`))
        .map(
          (f) =>
            store.readQuads(f, ns.persoon`datum`).next().value?.object?.value
        )[0];
      collect.geboorteDatum = geboorteDatum;

      const identifier = store
        .getObjects(persoon, ns.adms`identifier`)
        .filter((p) => store.has(p, ns.rdf`type`, ns.adms`Identifier`))
        .map(
          (f) =>
            store.readQuads(f, ns.skos`notation`).next().value?.object?.value
        )[0];
      collect.rrnummer = identifier;
    }

    const contact = store
      .getObjects(mandataris, ns.sch`contactPoint`)
      .filter((p) => store.has(p, ns.rdf`type`, ns.sch`ContactPoint`))[0];

    if (contact) {
      const contactSoort = store.readQuads(contact, ns.sch`contactType`).next()
        .value?.object?.value;
      const email = store.readQuads(contact, ns.sch`email`).next().value
        ?.object?.value;
      const telefoon = store.readQuads(contact, ns.sch`telephone`).next().value
        ?.object?.value;
      collect.contact = contact.value;
      collect.contactSoort = contactSoort;
      collect.email = email;
      collect.telefoon = telefoon;

      const adres = store
        .getObjects(contact, ns.locn`address`)
        .filter((a) => store.has(a, ns.rdf`type`, ns.locn`Address`))[0];

      if (adres) {
        const straat = store.readQuads(adres, ns.locn`thoroughfare`).next()
          .value?.object?.value;
        const huisnummer = store
          .readQuads(adres, ns.adres`Adresvoorstelling.huisnummer`)
          .next().value?.object?.value;
        const busnummer = store
          .readQuads(adres, ns.adres`Adresvoorstelling.busnummer`)
          .next().value?.object?.value;
        const postcode = store.readQuads(adres, ns.locn`postCode`).next().value
          ?.object?.value;
        const stad = store.readQuads(adres, ns.adres`gemeentenaam`).next().value
          ?.object?.value;
        const land = store.readQuads(adres, ns.adres`land`).next().value
          ?.object?.value;
        collect.adres = adres.value;
        collect.straat = straat;
        collect.huisnummer = huisnummer;
        collect.busnummer = busnummer;
        collect.postcode = postcode;
        collect.stad = stad;
        collect.land = land;
      }
    }

    const position = store
      .getObjects(mandataris, ns.org`holds`)
      .filter((p) => store.has(p, ns.rdf`type`, ns.mandaat`Mandaat`))[0];

    if (position) {
      const functienaam = store
        .getObjects(position, ns.org`role`)
        .filter((p) => store.has(p, ns.rdf`type`, ns.ext`BestuursfunctieCode`))
        .map(
          (f) =>
            store.readQuads(f, ns.skos`prefLabel`).next().value?.object?.value
        )[0];
      collect.rolnaam = functienaam;

      const besturenInTijd = store
        .getSubjects(ns.org`hasPost`, position)
        .filter((p) => store.has(p, ns.rdf`type`, ns.besluit`Bestuursorgaan`));

      for (const bestuurInTijd of besturenInTijd) {
        const bestuurInTijdStart = store
          .readQuads(bestuurInTijd, ns.mandaat`bindingStart`)
          .next().value?.object?.value;
        const bestuurInTijdEinde = store
          .readQuads(bestuurInTijd, ns.mandaat`bindingEinde`)
          .next().value?.object?.value;
        const bestuur = store
          .getObjects(bestuurInTijd, ns.mandaat`isTijdspecialisatieVan`)
          .filter((p) =>
            store.has(p, ns.rdf`type`, ns.besluit`Bestuursorgaan`)
          )[0];
        const bestuurnaam = store.readQuads(bestuur, ns.skos`prefLabel`).next()
          .value?.object?.value;
        const eenheid = store
          .getObjects(bestuur, ns.besluit`bestuurt`)
          .filter((p) =>
            store.has(p, ns.rdf`type`, ns.besluit`Bestuurseenheid`)
          )[0];
        const eenheidnaam = store.readQuads(eenheid, ns.skos`prefLabel`).next()
          .value?.object?.value;
        const typeEredienstLabel = store
          .getObjects(eenheid, ns.ere`typeEredienst`)
          .filter((p) =>
            store.has(p, ns.rdf`type`, ns.organ`TypeEredienst`)
          )
          .map(
            (f) =>
              store.readQuads(f, ns.skos`prefLabel`).next().value?.object?.value
          )[0];

        const collectBestuurInTijd = {
          bestuurInTijd: bestuurInTijd?.value,
          bestuurInTijdStart,
          bestuurInTijdEinde,
          bestuurnaam,
          eenheid: eenheid?.value,
          eenheidnaam,
          typeEredienstLabel,
        };

        data.push({ ...collect, ...collectBestuurInTijd });
      }
    } else {
      data.push(collect);
    }
  }
  return data;
}

export default {
  cronPattern: '20 1 * * *',
  name: 'eredienst-mandatarissen',
  execute: generate,
};
