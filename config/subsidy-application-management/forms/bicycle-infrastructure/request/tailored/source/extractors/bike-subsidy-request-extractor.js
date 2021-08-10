module.exports = {
  name: 'bike-subsidy-request-extractor',
  execute: async (store, graphs, lib, form) => {
    try {
      const { $rdf, mu, sudo } = lib;
      const SCHEMA = new $rdf.Namespace('http://schema.org/');
      const FOAF = new $rdf.Namespace('http://xmlns.com/foaf/0.1/');
      const LBLOD_SUBSIDIE = new $rdf.Namespace('http://lblod.data.gift/vocabularies/subsidie/');
      const BICYCLE_INFRASTRUCTURE = new $rdf.Namespace('http://lblod.data.gift/vocabularies/subsidie/bicycle-infrastructure#');
      const DCT = new $rdf.Namespace('http://purl.org/dc/terms/');
      const RDF = new $rdf.Namespace('http://www.w3.org/1999/02/22-rdf-syntax-ns#');
      const EXT = new $rdf.Namespace('http://mu.semte.ch/vocabularies/ext/');
      const URI_TEMPLATE = 'http://data.lblod.info/form-data/nodes/';

      const previousForm = await getPreviousForm(form.uri, mu, sudo);

      const info = await getPreviousStepInfo(previousForm, mu, sudo);

      // TODO
      // - Vérifier les données (objective table sauvegardé deux fois ..?
      // - Mettre un "disabled" pour certains fields comme le project name

      if (info) {
        const contactPointUri = new $rdf.NamedNode(URI_TEMPLATE + mu.uuid());

        store.add($rdf.sym(form.uri), LBLOD_SUBSIDIE('projectName'), info.projectName, graphs.additions);
        store.add($rdf.sym(form.uri), SCHEMA('contactPoint'), $rdf.sym(contactPointUri), graphs.additions);
        store.add($rdf.sym(contactPointUri), FOAF('firstName'), info.firstName, graphs.additions);
        store.add($rdf.sym(contactPointUri), FOAF('familyName'), info.familyName, graphs.additions);
        store.add($rdf.sym(contactPointUri), SCHEMA('email'), info.email, graphs.additions);
        store.add($rdf.sym(contactPointUri), SCHEMA('telephone'), info.telephone, graphs.additions);

        const estimatedCostEntriesInfo = await getEstimatedCostEntriesInfo(info.estimatedCostTable, mu, sudo);
        if (estimatedCostEntriesInfo && estimatedCostEntriesInfo.length) {
          const estimatedCostTableUri = new $rdf.NamedNode(URI_TEMPLATE + mu.uuid());
          estimatedCostEntriesInfo.forEach(estimatedCostEntryInfo => {
            const estimatedCostEntryUri = new $rdf.NamedNode(URI_TEMPLATE + mu.uuid());
            store.add($rdf.sym(form.uri), BICYCLE_INFRASTRUCTURE('estimatedCostTable'), $rdf.sym(estimatedCostTableUri), graphs.additions);
            store.add($rdf.sym(estimatedCostTableUri), BICYCLE_INFRASTRUCTURE('validEstimatedCostTable'), info.validEstimatedCostTable, graphs.additions);
            store.add($rdf.sym(estimatedCostTableUri), BICYCLE_INFRASTRUCTURE('estimatedCostEntry'), $rdf.sym(estimatedCostEntryUri), graphs.additions);
            store.add($rdf.sym(estimatedCostEntryUri), EXT('index'), estimatedCostEntryInfo.index, graphs.additions);
            store.add($rdf.sym(estimatedCostEntryUri), BICYCLE_INFRASTRUCTURE('costEstimationType'), estimatedCostEntryInfo.costEstimationType, graphs.additions);
            store.add($rdf.sym(estimatedCostEntryUri), BICYCLE_INFRASTRUCTURE('cost'), estimatedCostEntryInfo.cost, graphs.additions);
            store.add($rdf.sym(estimatedCostEntryUri), BICYCLE_INFRASTRUCTURE('share'), estimatedCostEntryInfo.share, graphs.additions);
          })
        }

        const objectiveEntriesInfo = await getObjectiveEntriesInfo(info.objectiveTable, mu, sudo);
        if (objectiveEntriesInfo && objectiveEntriesInfo.length) {
          const objectiveTableUri = new $rdf.NamedNode(URI_TEMPLATE + mu.uuid());
          objectiveEntriesInfo.forEach(objectiveEntryInfo => {
            const objectiveEntryUri = new $rdf.NamedNode(URI_TEMPLATE + mu.uuid());
            store.add($rdf.sym(form.uri), BICYCLE_INFRASTRUCTURE('objectiveTable'), $rdf.sym(objectiveTableUri), graphs.additions);
            store.add($rdf.sym(objectiveTableUri), BICYCLE_INFRASTRUCTURE('validObjectiveTable'), info.validObjectiveTable, graphs.additions);
            store.add($rdf.sym(objectiveTableUri), BICYCLE_INFRASTRUCTURE('objectiveEntry'), $rdf.sym(objectiveEntryUri), graphs.additions);
            store.add($rdf.sym(objectiveEntryUri), RDF('type'), $rdf.sym(objectiveEntryInfo.objectiveEntryType), graphs.additions);
            store.add($rdf.sym(objectiveEntryUri), BICYCLE_INFRASTRUCTURE('approachType'), objectiveEntryInfo.approachType, graphs.additions);
            store.add($rdf.sym(objectiveEntryUri), BICYCLE_INFRASTRUCTURE('directionType'), objectiveEntryInfo.directionType, graphs.additions);
            store.add($rdf.sym(objectiveEntryUri), BICYCLE_INFRASTRUCTURE('bikeLaneType'), objectiveEntryInfo.bikeLaneType, graphs.additions);
            store.add($rdf.sym(objectiveEntryUri), BICYCLE_INFRASTRUCTURE('kilometers'), objectiveEntryInfo.kilometers, graphs.additions);
          })
        }
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
    PREFIX bicycleInfrastructure: <http://lblod.data.gift/vocabularies/subsidie/bicycle-infrastructure#>
    PREFIX schema: <http://schema.org/>
    PREFIX dct: <http://purl.org/dc/terms/>
    PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>

    SELECT DISTINCT ?projectName ?decisionUpload ?estimatedCostTable ?objectiveTable ?contactPoint
                    ?firstName ?familyName ?email ?telephone ?validEstimatedCostTable ?validObjectiveTable
    WHERE {
      GRAPH ?g {
        ${mu.sparqlEscapeUri(formUri)}
          lblodSubsidie:projectName ?projectName ;
          lblodSubsidie:decisionUpload ?decisionUpload ;
          bicycleInfrastructure:estimatedCostTable ?estimatedCostTable ;
          bicycleInfrastructure:objectiveTable ?objectiveTable ;
          schema:contactPoint ?contactPoint .

        ?contactPoint schema:email ?email ;
          schema:telephone ?telephone ;
          foaf:firstName ?firstName ;
          foaf:familyName ?familyName .

        ?estimatedCostTable bicycleInfrastructure:validEstimatedCostTable ?validEstimatedCostTable .

        ?objectiveTable bicycleInfrastructure:validObjectiveTable ?validObjectiveTable .
      }
    }
  `);

  if (queryResult.results.bindings.length) {
    const projectName = queryResult.results.bindings[0].projectName.value;
    const decisionUpload = queryResult.results.bindings[0].decisionUpload.value;
    const estimatedCostTable = queryResult.results.bindings[0].estimatedCostTable.value;
    const validEstimatedCostTable = queryResult.results.bindings[0].validEstimatedCostTable.value;
    const objectiveTable = queryResult.results.bindings[0].objectiveTable.value;
    const validObjectiveTable = queryResult.results.bindings[0].validObjectiveTable.value;
    const contactPoint = queryResult.results.bindings[0].contactPoint.value;
    const firstName = queryResult.results.bindings[0].firstName.value;
    const familyName = queryResult.results.bindings[0].familyName.value;
    const email = queryResult.results.bindings[0].email.value;
    const telephone = queryResult.results.bindings[0].telephone.value;
    return {
      projectName,
      decisionUpload,
      estimatedCostTable,
      validEstimatedCostTable,
      objectiveTable,
      validObjectiveTable,
      contactPoint,
      firstName,
      familyName,
      email,
      telephone
    };
  }

  return null;
}

async function getEstimatedCostEntriesInfo(estimatedCostTableUri, mu, sudo) {
  const queryResult = await sudo.querySudo(`
    PREFIX bicycleInfrastructure: <http://lblod.data.gift/vocabularies/subsidie/bicycle-infrastructure#>
    PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>

    SELECT DISTINCT ?estimatedCostEntry ?index ?costEstimationType ?cost ?share
    WHERE {
      GRAPH ?g {
        ${mu.sparqlEscapeUri(estimatedCostTableUri)} bicycleInfrastructure:estimatedCostEntry ?estimatedCostEntry .
        ?estimatedCostEntry ext:index ?index ;
          bicycleInfrastructure:costEstimationType ?costEstimationType ;
          bicycleInfrastructure:cost ?cost ;
          bicycleInfrastructure:share ?share .
      }
    }
  `);

  if (queryResult.results.bindings.length) {
    return queryResult.results.bindings.map(binding => {
      return {
        estimatedCostEntry: binding.estimatedCostEntry.value,
        index: binding.index.value,
        costEstimationType: binding.costEstimationType.value,
        cost: binding.cost.value,
        share: binding.share.value,
      };
    })
  }

  return null;
}

async function getObjectiveEntriesInfo(objectiveTableUri, mu, sudo) {
  const queryResult = await sudo.querySudo(`
    PREFIX bicycleInfrastructure: <http://lblod.data.gift/vocabularies/subsidie/bicycle-infrastructure#>

    SELECT DISTINCT ?objectiveEntry ?objectiveEntryType ?approachType ?directionType ?bikeLaneType ?kilometers
    WHERE {
      GRAPH ?g {
        ${mu.sparqlEscapeUri(objectiveTableUri)} bicycleInfrastructure:objectiveEntry ?objectiveEntry .
        ?objectiveEntry a ?objectiveEntryType ;
          bicycleInfrastructure:approachType ?approachType ;
          bicycleInfrastructure:directionType ?directionType ;
          bicycleInfrastructure:bikeLaneType ?bikeLaneType ;
          bicycleInfrastructure:kilometers ?kilometers .
      }
    }
  `);

  if (queryResult.results.bindings.length) {
    return queryResult.results.bindings.map(binding => {
      return {
        objectiveEntry: binding.objectiveEntry.value,
        objectiveEntryType: binding.objectiveEntryType.value,
        approachType: binding.approachType.value,
        directionType: binding.directionType.value,
        bikeLaneType: binding.bikeLaneType.value,
        kilometers: binding.kilometers.value,
      };
    })
  }

  return null;
}
