import * as mas from '@lblod/mu-auth-sudo';
import * as utils from './utils';
import * as queries from './queries';
import { generateReportFromData } from '../../helpers';

const BATCH_SIZE = 100;

async function generate() {
  let collectedSubjects = [];
  let collectedData = [];

  const bindings = (await mas.querySudo(queries.allBedienaren())).results
    .bindings;
  const bedienarenURIs = bindings.map((b) => b.bedienaar.value);
  collectedSubjects = collectedSubjects.concat(bedienarenURIs);

  for (
    let batchCurrent = 0;
    batchCurrent < bedienarenURIs.length;
    batchCurrent += BATCH_SIZE
  ) {
    const subjects = bedienarenURIs.slice(
      batchCurrent,
      batchCurrent + BATCH_SIZE
    );
    const personenURIs = (
      await mas.querySudo(
        queries.subjectToRange(
          subjects,
          'http://www.w3.org/ns/org#heldBy',
          'http://www.w3.org/ns/person#Person'
        )
      )
    ).results.bindings.map((b) => b.range.value);
    collectedSubjects = collectedSubjects.concat(personenURIs);
    const contactURIs = (
      await mas.querySudo(
        queries.subjectToRange(
          subjects,
          'http://schema.org/contactPoint',
          'http://schema.org/ContactPoint'
        )
      )
    ).results.bindings.map((b) => b.range.value);
    collectedSubjects = collectedSubjects.concat(contactURIs);

    for (
      let batchCurrent = 0;
      batchCurrent < personenURIs.length;
      batchCurrent += BATCH_SIZE
    ) {
      const personenSlice = personenURIs.slice(
        batchCurrent,
        batchCurrent + BATCH_SIZE
      );
      const identifierURIs = (
        await mas.querySudo(
          queries.subjectToRange(
            personenSlice,
            'http://www.w3.org/ns/adms#identifier',
            'http://www.w3.org/ns/adms#Identifier'
          )
        )
      ).results.bindings.map((b) => b.range.value);
      collectedSubjects = collectedSubjects.concat(identifierURIs);
      const geboorteURIs = (
        await mas.querySudo(
          queries.subjectToRange(
            personenSlice,
            'http://data.vlaanderen.be/ns/persoon#heeftGeboorte',
            'http://data.vlaanderen.be/ns/persoon#Geboorte'
          )
        )
      ).results.bindings.map((b) => b.range.value);
      collectedSubjects = collectedSubjects.concat(geboorteURIs);
    }

    for (
      let batchCurrent = 0;
      batchCurrent < contactURIs.length;
      batchCurrent += BATCH_SIZE
    ) {
      const contactSlice = contactURIs.slice(
        batchCurrent,
        batchCurrent + BATCH_SIZE
      );
      const adresURIs = (
        await mas.querySudo(
          queries.subjectToRange(
            contactSlice,
            'http://www.w3.org/ns/locn#address',
            'http://www.w3.org/ns/locn#Address'
          )
        )
      ).results.bindings.map((b) => b.range.value);
      collectedSubjects = collectedSubjects.concat(adresURIs);
    }
  }

  for (
    let batchCurrent = 0;
    batchCurrent < collectedSubjects.length;
    batchCurrent += BATCH_SIZE
  ) {
    const subjects = collectedSubjects.slice(
      batchCurrent,
      batchCurrent + BATCH_SIZE
    );
    const data = (await mas.querySudo(queries.dataForSubjects(subjects)))
      .results.bindings;
    collectedData = collectedData.concat(data);
  }

  if (collectedData.length > 0) {
    collectedData = collectedData.map((res) => {
      return {
        graph: `<${res.g.value}>`,
        subject: `<${res.s.value}>`,
        predicate: `<${res.p.value}>`,
        object: utils.formatObjectTerm(res.o),
      };
    });
    await generateReportFromData(
      collectedData,
      ['graph', 'subject', 'predicate', 'object'],
      {
        title: 'Eredienst Bedienaren Report',
        description: 'All eredienst bedienaren and their information.',
        filePrefix: 'eredienst-bedienaren',
      }
    );
  }
}

export default {
  cronPattern: '0 1 * * *',
  name: 'eredienst-bedienaren',
  execute: generate,
};
