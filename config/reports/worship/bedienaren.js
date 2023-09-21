import * as mas from '@lblod/mu-auth-sudo';
import * as queries from './queries';
import * as hel from '../../helpers';

const BATCH_SIZE = 100;

async function generate() {
  const bedienarenResponse = await hel.batchedQuery(
    queries.allBedienaren(),
    BATCH_SIZE
  );
  const bedienaren = bedienarenResponse.results.bindings.sort(
    (a, b) => a.bedienaar.value < b.bedienaar.value
  );

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
    const bedienarenURIsSlice = bedienarenSlice.map((b) => b.bedienaar.value);

    //Personen
    const persoonQuery = queries.domainToRange(
      bedienarenURIsSlice,
      'http://www.w3.org/ns/org#heldBy',
      'http://www.w3.org/ns/person#Person'
    );
    const personenResponse = await mas.querySudo(persoonQuery);
    const personen = [
      ...new Set(personenResponse.results.bindings.map((p) => p.range.value)),
    ];
    const personenLinks = personenResponse.results.bindings.map((r) => {
      return {
        bedienaar: r.domain.value,
        persoon: r.range.value,
      };
    });

    //Identifiers
    const identifierQuery = queries.domainToRange(
      personen,
      'http://www.w3.org/ns/adms#identifier',
      'http://www.w3.org/ns/adms#Identifier'
    );
    const identifierResponse = await mas.querySudo(identifierQuery);
    const identifiers = [
      ...new Set(identifierResponse.results.bindings.map((i) => i.range.value)),
    ];
    const identifierLinks = identifierResponse.results.bindings.map((r) => {
      return {
        persoon: r.domain.value,
        identifier: r.range.value,
      };
    });

    //Geboortes
    const geboorteQuery = queries.domainToRange(
      personen,
      'http://data.vlaanderen.be/ns/persoon#heeftGeboorte',
      'http://data.vlaanderen.be/ns/persoon#Geboorte'
    );
    const geboortesResponse = await mas.querySudo(geboorteQuery);
    const geboortes = [
      ...new Set(geboortesResponse.results.bindings.map((g) => g.range.value)),
    ];
    const geboorteLinks = geboortesResponse.results.bindings.map((r) => {
      return {
        persoon: r.domain.value,
        geboorte: r.range.value,
      };
    });

    //Contacts
    const contactQuery = queries.domainToRange(
      bedienarenURIsSlice,
      'http://schema.org/contactPoint',
      'http://schema.org/ContactPoint'
    );
    const contactsResponse = await mas.querySudo(contactQuery);
    const contacts = [
      ...new Set(contactsResponse.results.bindings.map((c) => c.range.value)),
    ];
    const contactLinks = contactsResponse.results.bindings.map((r) => {
      return {
        bedienaar: r.domain.value,
        contact: r.range.value,
      };
    });

    //Adresses
    const adresQuery = queries.domainToRange(
      contacts,
      'http://www.w3.org/ns/locn#address',
      'http://www.w3.org/ns/locn#Address'
    );
    const addressResponse = await mas.querySudo(adresQuery);
    const addresses = [
      ...new Set(addressResponse.results.bindings.map((a) => a.range.value)),
    ];
    const addressLinks = addressResponse.results.bindings.map((r) => {
      return {
        contact: r.domain.value,
        address: r.range.value,
      };
    });

    //Positions
    const positionQuery = queries.domainToRange(
      bedienarenURIsSlice,
      'http://www.w3.org/ns/org#holds',
      'http://data.lblod.info/vocabularies/erediensten/PositieBedienaar'
    );
    const positionsResponse = await mas.querySudo(positionQuery);
    const positions = [
      ...new Set(positionsResponse.results.bindings.map((p) => p.range.value)),
    ];
    const positionLinks = positionsResponse.results.bindings.map((p) => {
      return {
        bedienaar: p.domain.value,
        position: p.range.value,
      };
    });

    //Functions
    const functionQuery = queries.domainToRange(
      positions,
      'http://data.lblod.info/vocabularies/erediensten/functie',
      'http://lblod.data.gift/vocabularies/organisatie/EredienstBeroepen'
    );
    const functionResponse = await mas.querySudo(functionQuery);
    const functions = [
      ...new Set(functionResponse.results.bindings.map((p) => p.range.value)),
    ];
    const functionLinks = functionResponse.results.bindings.map((p) => {
      return {
        position: p.domain.value,
        functie: p.range.value,
      };
    });

    //Besturen
    const besturenQuery = queries.rangeToDomain(
      positions,
      'http://data.lblod.info/vocabularies/erediensten/wordtBediendDoor',
      'http://data.lblod.info/vocabularies/erediensten/BestuurVanDeEredienst'
    );
    const besturenResponse = await mas.querySudo(besturenQuery);
    const besturen = [
      ...new Set(besturenResponse.results.bindings.map((p) => p.range.value)),
    ];
    const besturenLinks = besturenResponse.results.bindings.map((p) => {
      return {
        positie: p.domain.value,
        bestuur: p.range.value,
      };
    });

    let allURIs = new Set();
    [
      ...bedienarenURIsSlice,
      ...personen,
      ...identifiers,
      ...geboortes,
      ...contacts,
      ...addresses,
      ...positions,
      ...functions,
      ...besturen,
    ].forEach((e) => allURIs.add(e));
    allURIs = [...allURIs];

    const tripleData = [];

    for (let counter = 0; counter < allURIs.length; counter += BATCH_SIZE) {
      const URIslice = allURIs.slice(counter, counter + BATCH_SIZE);
      const dataQuery = queries.dataForSubjects(URIslice);
      const dataResponse = await mas.querySudo(dataQuery);
      dataResponse.results.bindings.forEach((e) => tripleData.push(e));
    }

    const combinedData = combineBedienarenData(
      tripleData,
      bedienarenURIsSlice,
      personenLinks,
      identifierLinks,
      geboorteLinks,
      contactLinks,
      addressLinks,
      positionLinks,
      functionLinks,
      besturenLinks
    );
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

function combineBedienarenData(
  tripleData,
  bedienaren,
  personenLinks,
  identifierLinks,
  geboorteLinks,
  contactLinks,
  addressLinks,
  positionLinks,
  functionLinks,
  besturenLinks
) {
  const data = [];

  for (const bedienaar of bedienaren) {
    const afkomstGegevens = tripleData.find(
      (d) =>
        d.s.value === bedienaar &&
        d.p.value === 'http://www.w3.org/ns/prov#wasGeneratedBy'
    )?.o?.value;
    const startDatum = tripleData.find(
      (d) =>
        d.s.value === bedienaar &&
        d.p.value ===
          'http://data.lblod.info/vocabularies/contacthub/startdatum'
    )?.o?.value;
    const eindeDatum = tripleData.find(
      (d) =>
        d.s.value === bedienaar &&
        d.p.value ===
          'http://data.lblod.info/vocabularies/contacthub/eindedatum'
    )?.o?.value;

    const collect = { bedienaar, afkomstGegevens, startDatum, eindeDatum };

    const positionLink = positionLinks.find((p) => p.bedienaar === bedienaar);

    if (positionLink) {
      const functionLink = functionLinks.find(
        (f) => f.position === positionLink.position
      );
      if (functionLink) {
        const functienaam = tripleData.find(
          (d) =>
            d.s.value === functionLink.functie &&
            d.p.value === 'http://www.w3.org/2004/02/skos/core#prefLabel'
        )?.o?.value;
        collect.functienaam = functienaam;
      }

      const bestuurLink = besturenLinks.find(
        (b) => positionLink.position === b.positie
      );
      if (bestuurLink) {
        const bestuurnaam = tripleData.find(
          (d) =>
            d.s.value === bestuurLink.bestuur &&
            d.p.value === 'http://www.w3.org/2004/02/skos/core#prefLabel'
        )?.o?.value;
        collect.bestuurnaam = bestuurnaam;
      }
    }

    const personen = personenLinks.filter((p) => p.bedienaar === bedienaar);

    for (const persoonLink of personen) {
      const familienaam = tripleData.find(
        (d) =>
          d.s.value === persoonLink.persoon &&
          d.p.value === 'http://xmlns.com/foaf/0.1/familyName'
      )?.o?.value;
      const voornaam = tripleData.find(
        (d) =>
          d.s.value === persoonLink.persoon &&
          d.p.value === 'http://data.vlaanderen.be/ns/persoon#gebruikteVoornaam'
      )?.o?.value;
      const nationaliteit = tripleData.find(
        (d) =>
          d.s.value === persoonLink.persoon &&
          d.p.value ===
            'http://data.vlaanderen.be/ns/persoon#heeftNationaliteit'
      )?.o?.value;
      const geslacht = tripleData.find(
        (d) =>
          d.s.value === persoonLink.persoon &&
          d.p.value === 'http://data.vlaanderen.be/ns/persoon#geslacht'
      )?.o?.value;
      collect.persoon = persoonLink.persoon;
      collect.familienaam = familienaam;
      collect.voornaam = voornaam;
      collect.nationaliteit = nationaliteit;
      collect.geslacht = geslacht;

      const geboorteLink =
        geboorteLinks.find((g) => g.persoon === persoonLink.persoon) || [];
      if (geboorteLink) {
        const geboorteDatum = tripleData.find(
          (d) =>
            d.s.value === geboorteLink.geboorte &&
            d.p.value === 'http://data.vlaanderen.be/ns/persoon#datum'
        )?.o?.value;
        collect.geboorteDatum = geboorteDatum;
      }

      const identifierLink =
        identifierLinks.find((g) => g.persoon === persoonLink.persoon) || [];
      if (identifierLink) {
        const identifier = tripleData.find(
          (d) =>
            d.s.value === identifierLink.identifier &&
            d.p.value === 'http://www.w3.org/2004/02/skos/core#notation'
        )?.o?.value;
        collect.rrnummer = identifier;
      }

      const contactLink = contactLinks.find((c) => c.bedienaar === bedienaar);
      if (contactLink) {
        const contactSoort = tripleData.find(
          (d) =>
            d.s.value === contactLink.contact &&
            d.p.value === 'http://schema.org/contactType'
        )?.o?.value;
        const email = tripleData.find(
          (d) =>
            d.s.value === contactLink.contact &&
            d.p.value === 'http://schema.org/email'
        )?.o?.value;
        const telefoon = tripleData.find(
          (d) =>
            d.s.value === contactLink.contact &&
            d.p.value === 'http://schema.org/telephone'
        )?.o?.value;
        collect.contact = contactLink.contact;
        collect.contactSoort = contactSoort;
        collect.email = email;
        collect.telefoon = telefoon;

        const adresLink = addressLinks.find(
          (c) => c.contact === contactLink.contact
        );
        if (adresLink) {
          const straat = tripleData.find(
            (d) =>
              d.s.value === adresLink.address &&
              d.p.value === 'http://www.w3.org/ns/locn#thoroughfare'
          )?.o?.value;
          const huisnummer = tripleData.find(
            (d) =>
              d.s.value === adresLink.address &&
              d.p.value ===
                'https://data.vlaanderen.be/ns/adres#Adresvoorstelling.huisnummer'
          )?.o?.value;
          const busnummer = tripleData.find(
            (d) =>
              d.s.value === adresLink.address &&
              d.p.value ===
                'https://data.vlaanderen.be/ns/adres#Adresvoorstelling.busnummer'
          )?.o?.value;
          const postcode = tripleData.find(
            (d) =>
              d.s.value === adresLink.address &&
              d.p.value === 'http://www.w3.org/ns/locn#postCode'
          )?.o?.value;
          const stad = tripleData.find(
            (d) =>
              d.s.value === adresLink.address &&
              d.p.value === 'https://data.vlaanderen.be/ns/adres#gemeentenaam'
          )?.o?.value;
          const land = tripleData.find(
            (d) =>
              d.s.value === adresLink.address &&
              d.p.value === 'https://data.vlaanderen.be/ns/adres#land'
          )?.o?.value;
          collect.adres = adresLink.address;
          collect.straat = straat;
          collect.huisnummer = huisnummer;
          collect.busnummer = busnummer;
          collect.postcode = postcode;
          collect.stad = stad;
          collect.land = land;
        }
      }

      data.push(collect);
    }
  }
  return data;
}

export default {
  cronPattern: '0 1 * * *',
  name: 'eredienst-bedienaren',
  execute: generate,
};
