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

      const previousForm = await getPreviousForm(form.uri, mu, sudo);

      const info = await getPreviousStepInfo(previousForm, mu, sudo);
      if (info) {
        store.add($rdf.sym(form.uri), LBLOD_SUBSIDIE('projectName'), info.projectName, graphs.additions);
        store.add($rdf.sym(form.uri), LBLOD_SUBSIDIE('decisionUpload'), $rdf.sym(info.decisionUpload), graphs.additions);
        store.add($rdf.sym(form.uri), BICYCLE_INFRASTRUCTURE('estimatedCostTable'), $rdf.sym(info.estimatedCostTable), graphs.additions);
        store.add($rdf.sym(info.estimatedCostTable), BICYCLE_INFRASTRUCTURE('validEstimatedCostTable'), info.validEstimatedCostTable, graphs.additions);
        store.add($rdf.sym(form.uri), BICYCLE_INFRASTRUCTURE('objectiveTable'), $rdf.sym(info.objectiveTable), graphs.additions);
        store.add($rdf.sym(info.objectiveTable), BICYCLE_INFRASTRUCTURE('validObjectiveTable'), info.validObjectiveTable, graphs.additions);
        store.add($rdf.sym(form.uri), SCHEMA('contactPoint'), $rdf.sym(info.contactPoint), graphs.additions);
        store.add($rdf.sym(info.contactPoint), FOAF('firstName'), info.firstName, graphs.additions);
        store.add($rdf.sym(info.contactPoint), FOAF('familyName'), info.familyName, graphs.additions);
        store.add($rdf.sym(info.contactPoint), SCHEMA('email'), info.email, graphs.additions);
        store.add($rdf.sym(info.contactPoint), SCHEMA('telephone'), info.telephone, graphs.additions);

        const filesInfo = await getFilesInfo(info.decisionUpload, mu, sudo);
        if (filesInfo && filesInfo.length) {
          filesInfo.forEach(fileInfo => {
            store.add($rdf.sym(info.decisionUpload), DCT('hasPart'), $rdf.sym(fileInfo.decisionUploadFile), graphs.additions);
            store.add($rdf.sym(fileInfo.decisionUploadFile), RDF('type'), $rdf.sym(fileInfo.decisionUploadFileType), graphs.additions);
          })
        }

        const estimatedCostEntriesInfo = await getEstimatedCostEntriesInfo(info.estimatedCostTable, mu, sudo);
        if (estimatedCostEntriesInfo && estimatedCostEntriesInfo.length) {
          estimatedCostEntriesInfo.forEach(estimatedCostEntryInfo => {
            store.add($rdf.sym(info.estimatedCostTable), BICYCLE_INFRASTRUCTURE('estimatedCostEntry'), $rdf.sym(estimatedCostEntryInfo.estimatedCostEntry), graphs.additions);
            store.add($rdf.sym(estimatedCostEntryInfo.estimatedCostEntry), EXT('index'), estimatedCostEntryInfo.index, graphs.additions);
            store.add($rdf.sym(estimatedCostEntryInfo.estimatedCostEntry), BICYCLE_INFRASTRUCTURE('costEstimationType'), estimatedCostEntryInfo.costEstimationType, graphs.additions);
            store.add($rdf.sym(estimatedCostEntryInfo.estimatedCostEntry), BICYCLE_INFRASTRUCTURE('cost'), estimatedCostEntryInfo.cost, graphs.additions);
            store.add($rdf.sym(estimatedCostEntryInfo.estimatedCostEntry), BICYCLE_INFRASTRUCTURE('share'), estimatedCostEntryInfo.share, graphs.additions);
          })
        }

        const objectiveEntriesInfo = await getObjectiveEntriesInfo(info.objectiveTable, mu, sudo);
        if (objectiveEntriesInfo && objectiveEntriesInfo.length) {
          objectiveEntriesInfo.forEach(objectiveEntryInfo => {
            store.add($rdf.sym(info.objectiveTable), BICYCLE_INFRASTRUCTURE('objectiveEntry'), $rdf.sym(objectiveEntryInfo.objectiveEntry), graphs.additions);
            store.add($rdf.sym(objectiveEntryInfo.objectiveEntry), RDF('type'), objectiveEntryInfo.objectiveEntryType, graphs.additions);
            store.add($rdf.sym(objectiveEntryInfo.objectiveEntry), BICYCLE_INFRASTRUCTURE('approachType'), objectiveEntryInfo.approachType, graphs.additions);
            store.add($rdf.sym(objectiveEntryInfo.objectiveEntry), BICYCLE_INFRASTRUCTURE('directionType'), objectiveEntryInfo.directionType, graphs.additions);
            store.add($rdf.sym(objectiveEntryInfo.objectiveEntry), BICYCLE_INFRASTRUCTURE('bikeLaneType'), objectiveEntryInfo.bikeLaneType, graphs.additions);
            store.add($rdf.sym(objectiveEntryInfo.objectiveEntry), BICYCLE_INFRASTRUCTURE('kilometers'), objectiveEntryInfo.kilometers, graphs.additions);
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

async function getFilesInfo(decisionUploadUri, mu, sudo) {
  const queryResult = await sudo.querySudo(`
    PREFIX lblodSubsidie: <http://lblod.data.gift/vocabularies/subsidie/>
    PREFIX bicycleInfrastructure: <http://lblod.data.gift/vocabularies/subsidie/bicycle-infrastructure#>
    PREFIX schema: <http://schema.org/>
    PREFIX dct: <http://purl.org/dc/terms/>
    PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>

    SELECT DISTINCT ?decisionUploadFile ?decisionUploadFileType
    WHERE {
      GRAPH ?g {
        ${mu.sparqlEscapeUri(decisionUploadUri)} dct:hasPart ?decisionUploadFile .
        ?decisionUploadFile a ?decisionUploadFileType .
      }
    }
  `);

  if (queryResult.results.bindings.length) {
    return queryResult.results.bindings.map(binding => {
      return {
        decisionUploadFile: binding.decisionUploadFile.value,
        decisionUploadFileType: binding.decisionUploadFileType.value
      };
    })
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
