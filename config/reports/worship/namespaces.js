import * as n3 from 'n3';
const { namedNode } = n3.DataFactory;

const PREFIXES = {
  rdf: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#',
  xsd: 'http://www.w3.org/2001/XMLSchema#',
  mu: 'http://mu.semte.ch/vocabularies/core/',
  ere: 'http://data.lblod.info/vocabularies/erediensten/',
  prov: 'http://www.w3.org/ns/prov#',
  contact: 'http://data.lblod.info/vocabularies/contacthub/',
  org: 'http://www.w3.org/ns/org#',
  skos: 'http://www.w3.org/2004/02/skos/core#',
  organ: 'http://lblod.data.gift/vocabularies/organisatie/',
  person: 'http://www.w3.org/ns/person#',
  foaf: 'http://xmlns.com/foaf/0.1/',
  persoon: 'http://data.vlaanderen.be/ns/persoon#',
  adms: 'http://www.w3.org/ns/adms#',
  sch: 'http://schema.org/',
  locn: 'http://www.w3.org/ns/locn#',
  adres: 'https://data.vlaanderen.be/ns/adres#',
  mandaat: 'http://data.vlaanderen.be/ns/mandaat#',
  ext: 'http://mu.semte.ch/vocabularies/ext/',
  besluit: 'http://data.vlaanderen.be/ns/besluit#',
};

export const NAMESPACES = (() => {
  const all = {};
  for (const key in PREFIXES)
    all[key] = (pred) => namedNode(`${PREFIXES[key]}${pred}`);
  return all;
})();
