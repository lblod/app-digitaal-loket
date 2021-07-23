module.exports = {
  name: 'bike-subsidy-request-extractor',
  execute: async (store, graphs, lib, form) => {
    const { $rdf, mu, sudo } = lib;
    const SCHEMA = new $rdf.Namespace('http://schema.org/');
    const FOAF = new $rdf.Namespace('http://xmlns.com/foaf/0.1/');
    const LBLOD_SUBSIDIE = new $rdf.Namespace('http://lblod.data.gift/vocabularies/subsidie/');
    const BICYCLE_INFRASTRUCTURE = new $rdf.Namespace('http://lblod.data.gift/vocabularies/subsidie/bicycle-infrastructure#');

    const previousForm = await getPreviousForm(form.uri, mu, sudo);

    const info = await getPreviousStepInfo(previousForm, mu, sudo);
    console.log('INFOOOOOOOOOOOOOOOOOO', info)
    if (info) {
      store.add($rdf.sym(form.uri), LBLOD_SUBSIDIE('projectName'), info.projectName, graphs.additions);
      store.add($rdf.sym(form.uri), LBLOD_SUBSIDIE('decisionUpload'), info.decisionUpload, graphs.additions);
      store.add($rdf.sym(form.uri), BICYCLE_INFRASTRUCTURE('estimatedCostTable'), info.estimatedCostTable, graphs.additions);
      store.add($rdf.sym(form.uri), BICYCLE_INFRASTRUCTURE('objectiveTable'), info.objectiveTable, graphs.additions);
      store.add($rdf.sym(form.uri), SCHEMA('contactPoint'), info.contactPoint, graphs.additions);
      store.add(info.contactPoint, FOAF('firstName'), info.firstName, graphs.additions);
      store.add(info.contactPoint, FOAF('lastName'), info.lastName, graphs.additions);
      store.add(info.contactPoint, SCHEMA('email'), info.email, graphs.additions);
      store.add(info.contactPoint, SCHEMA('phone'), info.phone, graphs.additions);
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
    PREFIX bicycleInfrastructure: <http://lblod.data.gift/vocabularies/subsidie/bicycle-infrastructure#>
    PREFIX schema: <http://schema.org/>

    SELECT DISTINCT ?projectName ?decisionUpload ?estimatedCostTable ?objectiveTable ?contactPoint
                    ?firstName ?lastName ?email ?phone 
    WHERE {
      GRAPH ?g {
        ${mu.sparqlEscapeUri(formUri)}
          lblodSubsidie:projectName ?projectName ;
          lblodSubsidie:decisionUpload ?decisionUpload ;
          bicycleInfrastructure:estimatedCostTable ?estimatedCostTable ;
          bicycleInfrastructure:objectiveTable ?objectiveTable ;
          schema:contactPoint ?contactPoint .

        ?contactPoint schema:email ?email ;
          schema:telephone ?phone ;
          foaf:firstName ?firstName ;
          foaf:familyName ?lastName .
      }
    }
  `);

  if (queryResult.results.bindings.length) {
    const projectName = queryResult.results.bindings[0].projectName.value;
    const decisionUpload = queryResult.results.bindings[0].decisionUpload.value;
    const estimatedCostTable = queryResult.results.bindings[0].estimatedCostTable.value;
    const objectiveTable = queryResult.results.bindings[0].objectiveTable.value;
    const contactPoint = queryResult.results.bindings[0].contactPoint.value;
    const firstName = queryResult.results.bindings[0].firstName.value;
    const lastName = queryResult.results.bindings[0].lastName.value;
    const email = queryResult.results.bindings[0].email.value;
    const phone = queryResult.results.bindings[0].phone.value;
    return {
      projectName,
      decisionUpload,
      estimatedCostTable,
      objectiveTable,
      contactPoint,
      firstName,
      lastName,
      email,
      phone
    };
  }

  return null;
}
