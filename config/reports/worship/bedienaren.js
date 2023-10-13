import * as mas from '@lblod/mu-auth-sudo';
import * as queries from './queries';
import * as hel from '../../helpers';
import * as n3 from 'n3';
import * as uti from './utils';
import { NAMESPACES as ns } from './namespaces';
import { SparqlJsonParser } from 'sparqljson-parse';
const sparqlJsonParser = new SparqlJsonParser();

const BATCH_SIZE = 20;
const connectionOptions = {
  sparqlEndpoint: 'http://virtuoso:8890/sparql',
  mayRetry: true,
};

async function generate() {
  const bedienarenResponse = await hel.batchedQuery(
    queries.allBedienaren(),
    BATCH_SIZE
  );
  const bedienaren = sparqlJsonParser
    .parseJsonResults(bedienarenResponse)
    .map((b) => b.bedienaar)
    .sort((a, b) => a.value < b.value);

  let allData = [];

  for (
    let bedienaarCounter = 0;
    bedienaarCounter < bedienaren.length;
    bedienaarCounter += BATCH_SIZE
  ) {
    const bedienarenSlice = bedienaren.slice(
      bedienaarCounter,
      bedienaarCounter + BATCH_SIZE
    );

    //Personen
    const persoonQuery = queries.domainToRange(
      bedienarenSlice,
      ns.org`heldBy`,
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
      bedienarenSlice,
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
      bedienarenSlice,
      ns.org`holds`,
      ns.ere`PositieBedienaar`
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
      ns.ere`functie`,
      ns.organ`EredienstBeroepen`
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
    const besturenQuery = queries.rangeToDomain(
      positions,
      ns.ere`wordtBediendDoor`,
      ns.ere`BestuurVanDeEredienst`
    );
    const besturenResponse = await mas.querySudo(
      besturenQuery,
      undefined,
      connectionOptions
    );
    const besturen = sparqlJsonParser
      .parseJsonResults(besturenResponse)
      .map((i) => i.range);

    const allSubjects = uti.dedup(
      [
        ...bedienarenSlice,
        ...personen,
        ...identifiers,
        ...geboortes,
        ...contacts,
        ...addresses,
        ...positions,
        ...functions,
        ...besturen,
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
      dataParsed.forEach((e) => tripleData.addQuad(e.s, e.p, e.o, e.g));
    }

    const combinedData = combineBedienarenData(tripleData);
    allData = allData.concat(combinedData);
  }

  await hel.generateReportFromData(
    allData,
    [
      'bestuurnaam',
      'bedienaar',
      'afkomstGegevens',
      'functienaam',
      'startDatum',
      'eindeDatum',
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
      title: 'Eredienst Bedienaren Report',
      description: 'All eredienst bedienaren and their information.',
      filePrefix: 'eredienst-bedienaren',
    }
  );
}

function combineBedienarenData(store) {
  const data = [];
  const bedienaren = store.getSubjects(ns.rdf`type`, ns.ere`RolBedienaar`);

  for (const bedienaar of bedienaren) {
    const afkomstGegevens = store
      .readQuads(bedienaar, ns.prov`wasGeneratedBy`)
      .next().value?.object?.value;
    const startDatum = store.readQuads(bedienaar, ns.contact`startdatum`).next()
      .value?.object?.value;
    const eindeDatum = store.readQuads(bedienaar, ns.contact`eindedatum`).next()
      .value?.object?.value;

    const collect = {
      bedienaar: bedienaar.value,
      afkomstGegevens,
      startDatum,
      eindeDatum,
    };

    const persoon = store
      .getObjects(bedienaar, ns.org`heldBy`)
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
      .getObjects(bedienaar, ns.sch`contactPoint`)
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
      .getObjects(bedienaar, ns.org`holds`)
      .filter((p) => store.has(p, ns.rdf`type`, ns.ere`PositieBedienaar`))[0];

    if (position) {
      const functienaam = store
        .getObjects(position, ns.ere`functie`)
        .filter((p) => store.has(p, ns.rdf`type`, ns.organ`EredienstBeroepen`))
        .map(
          (f) =>
            store.readQuads(f, ns.skos`prefLabel`).next().value?.object?.value
        )[0];
      collect.functienaam = functienaam;

      const bestuurnaam = store
        .getSubjects(ns.ere`wordtBediendDoor`, position)
        .filter((p) =>
          store.has(p, ns.rdf`type`, ns.ere`BestuurVanDeEredienst`)
        )
        .map(
          (b) =>
            store.readQuads(b, ns.skos`prefLabel`).next().value?.object?.value
        )[0];
      collect.bestuurnaam = bestuurnaam;
    }
    data.push(collect);
  }
  return data;
}

export default {
  cronPattern: '0 1 * * *',
  name: 'eredienst-bedienaren',
  execute: generate,
};
