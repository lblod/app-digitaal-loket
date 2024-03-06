module.exports = {
  name: 'bike-subsidy-request-extractor',
  execute: async (store, graphs, lib, form) => {
    try {
      const { $rdf, mu, sudo } = lib;

      const RDF_TYPE = new $rdf.NamedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type');
      const SCHEMA = new $rdf.Namespace('http://schema.org/');
      const FOAF = new $rdf.Namespace('http://xmlns.com/foaf/0.1/');
      const LBLOD_SUBSIDIE = new $rdf.Namespace('http://lblod.data.gift/vocabularies/subsidie/');
      const NIE = new $rdf.Namespace('http://www.semanticdesktop.org/ontologies/2007/01/19/nie#');
      const URI_TEMPLATE = 'http://data.lblod.info/form-data/nodes/';

      const previousForm = await getPreviousForm(form.uri, mu, sudo);

      const info = await getPreviousStepInfo(previousForm, mu, sudo);

      if (info) {
        const contactPointUri = new $rdf.NamedNode(URI_TEMPLATE + mu.uuid());

        store.add($rdf.sym(form.uri), LBLOD_SUBSIDIE('projectName'), info.projectName, graphs.additions);
        store.add($rdf.sym(form.uri), NIE('identifier'), info.dossiernummer, graphs.additions);
        store.add($rdf.sym(form.uri), SCHEMA('contactPoint'), $rdf.sym(contactPointUri), graphs.additions);
        store.add($rdf.sym(contactPointUri), RDF_TYPE, SCHEMA('ContactPoint'), graphs.additions);
        store.add($rdf.sym(contactPointUri), FOAF('firstName'), info.firstName, graphs.additions);
        store.add($rdf.sym(contactPointUri), FOAF('familyName'), info.familyName, graphs.additions);
        store.add($rdf.sym(contactPointUri), SCHEMA('email'), info.email, graphs.additions);
        store.add($rdf.sym(contactPointUri), SCHEMA('telephone'), info.telephone, graphs.additions);
      }
    } catch (e) {
      console.log("An error occured while extracting the bike subsidy metadata: ", e)
    }
  }
};

// -------------------------------- INTERNAL LOGIC --------------------------------

async function getPreviousForm(formUri, mu, sudo) {
  const queryResult = await sudo.querySudo(`
    PREFIX dct: <http://purl.org/dc/terms/>
    PREFIX xkos: <http://rdf-vocabulary.ddialliance.org/xkos#>

    SELECT DISTINCT ?previousForm where {
      GRAPH ?g {
        ${mu.sparqlEscapeUri(formUri)} dct:isPartOf ?currentStep .
        ?smc dct:source ${mu.sparqlEscapeUri(formUri)} .
        ?previousForm dct:isPartOf ?previousStep .
        ?smc dct:source ?previousForm .
      }
      GRAPH ?h {
        ?currentStep xkos:previous ?previousStep .
      }
    }
  `);

  if (queryResult.results.bindings.length) {
    const previousForm = queryResult.results.bindings[0].previousForm.value;
    return previousForm;
  }

  return null;
}

async function getPreviousStepInfo(formUri, mu, sudo) {
  const queryResult = await sudo.querySudo(`
    PREFIX lblodSubsidie: <http://lblod.data.gift/vocabularies/subsidie/>
    PREFIX schema: <http://schema.org/>
    PREFIX nie: <http://www.semanticdesktop.org/ontologies/2007/01/19/nie#>

    SELECT DISTINCT ?projectName ?dossiernummer ?contactPoint ?firstName ?familyName ?email ?telephone
    WHERE {
      GRAPH ?g {
        ${mu.sparqlEscapeUri(formUri)}
          lblodSubsidie:projectName ?projectName ;
          nie:identifier ?dossiernummer ;
          schema:contactPoint ?contactPoint .

        ?contactPoint schema:email ?email ;
          schema:telephone ?telephone ;
          foaf:firstName ?firstName ;
          foaf:familyName ?familyName .
      }
    }
  `);

  if (queryResult.results.bindings.length) {
    const projectName = queryResult.results.bindings[0].projectName.value;
    const dossiernummer = queryResult.results.bindings[0].dossiernummer.value;
    const contactPoint = queryResult.results.bindings[0].contactPoint.value;
    const firstName = queryResult.results.bindings[0].firstName.value;
    const familyName = queryResult.results.bindings[0].familyName.value;
    const email = queryResult.results.bindings[0].email.value;
    const telephone = queryResult.results.bindings[0].telephone.value;
    return {
      projectName,
      dossiernummer,
      contactPoint,
      firstName,
      familyName,
      email,
      telephone
    };
  }

  return null;
}
