import * as mas from '@lblod/mu-auth-sudo';
import * as queries from './queries';
import { generateReportFromData } from '../../helpers';

async function generate() {
  const data = await mas.querySudo(queries.getQueryBedienaren());
  const bindings = data?.results?.bindings || [];
  const stringified = bindings.map((binding) => {
    const res = {};
    for (const key in binding) res[key] = binding[key].value;
    return res;
  });
  stringified.sort((a, b) => a.bedienaar < b.bedienaar);
  await generateReportFromData(
    stringified,
    [
      'bestuurnaam',
      'bedienaar',
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
      'busnummer',
      'huisnummer',
      'straat',
      'postcode',
      'stad',
      'land',
      'volAdress',
      'adresVerwijzing',
    ],
    {
      title: 'Eredienst Bedienaren Report',
      description: 'All eredienst bedienaren and their information.',
      filePrefix: 'eredienst-bedienaren',
    }
  )
}

export default {
  cronPattern: '0 1 * * *',
  name: 'eredienst-bedienaren',
  execute: generate,
};
