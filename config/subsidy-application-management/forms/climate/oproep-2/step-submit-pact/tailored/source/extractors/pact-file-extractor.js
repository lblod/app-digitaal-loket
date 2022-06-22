const fs = require("fs");
const URI_BASE = "http://data.lblod.info/form-data/nodes/";
const FILE_SERVICE = "http://mu.semte.ch/services/file-service/files/";

module.exports = {
  name: "climate-subsidy/submit-pact/pact-file-extractor",
  execute: async (store, graphs, lib, target, source) => {
    if (!source) source = target;

    const { $rdf, mu, sudo } = lib;

    const NFO = new $rdf.Namespace(
      "http://www.semanticdesktop.org/ontologies/2007/03/22/nfo#"
    );
    const NIE = new $rdf.Namespace(
      "http://www.semanticdesktop.org/ontologies/2007/01/19/nie#"
    );
    const DBPEDIA = new $rdf.Namespace("http://dbpedia.org/ontology/");
    const DCT = new $rdf.Namespace("http://purl.org/dc/terms/");
    const LBLODDATA = new $rdf.Namespace(
      "http://lblod.data.gift/vocabularies/subsidie/"
    );
    const W3 = new $rdf.Namespace(
      "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
    );
    const CORE = new $rdf.Namespace("http://mu.semte.ch/vocabularies/core/");

    const signedPact = new $rdf.NamedNode(URI_BASE + mu.uuid());

    store.add(
      $rdf.sym(target.uri),
      LBLODDATA("signedPact"),
      $rdf.sym(signedPact),
      graphs.additions
    );

    const {
      oldFileService,
      type,
      created,
      modified,
      format,
      fileName,
      fileSize,
      fileExtension,
    } = await getUploadResources(source.uri, mu, sudo);

    if (oldFileService) {
      const fileServiceUuid = mu.uuid();
      const fileService = new $rdf.NamedNode(FILE_SERVICE + fileServiceUuid);

      store.add(
        $rdf.sym(signedPact),
        DCT("hasPart"),
        $rdf.sym(fileService),
        graphs.additions
      );

      store.add(
        $rdf.sym(fileService),
        W3("type"),
        type.value,
        graphs.additions
      );

      store.add(
        $rdf.sym(fileService),
        CORE("uuid"),
        fileServiceUuid,
        graphs.additions
      );

      store.add(
        $rdf.sym(fileService),
        DCT("created"),
        created.value,
        graphs.additions
      );

      store.add(
        $rdf.sym(fileService),
        DCT("modified"),
        modified.value,
        graphs.additions
      );

      store.add(
        $rdf.sym(fileService),
        DCT("format"),
        format.value,
        graphs.additions
      );

      store.add(
        $rdf.sym(fileService),
        NFO("fileName"),
        fileName.value,
        graphs.additions
      );

      store.add(
        $rdf.sym(fileService),
        NFO("fileSize"),
        fileSize.value,
        graphs.additions
      );

      store.add(
        $rdf.sym(fileService),
        DBPEDIA("fileExtension"),
        fileExtension.value,
        graphs.additions
      );

      const {
          oldFileLocation,
        oldFileName,
        oldFileUuid
      } = await getFileResources(source.uri, mu, sudo);



      store.add(
        $rdf.sym(oldFileLocation.value),
        W3("type"),
        type.value,
        graphs.additions
      );

      store.add(
        $rdf.sym(oldFileLocation.value),
        CORE("uuid"),
        oldFileUuid,
        graphs.additions
      );

      store.add(
        $rdf.sym(oldFileLocation.value),
        DCT("created"),
        created.value,
        graphs.additions
      );

      store.add(
        $rdf.sym(oldFileLocation.value),
        DCT("modified"),
        modified.value,
        graphs.additions
      );

      store.add(
        $rdf.sym(oldFileLocation.value),
        DCT("format"),
        format.value,
        graphs.additions
      );

      store.add(
        $rdf.sym(oldFileLocation.value),
        NFO("fileName"),
        oldFileName.value,
        graphs.additions
      );

      store.add(
        $rdf.sym(oldFileLocation.value),
        NFO("fileSize"),
        fileSize.value,
        graphs.additions
      );

      store.add(
        $rdf.sym(oldFileLocation.value),
        DBPEDIA("fileExtension"),
        fileExtension.value,
        graphs.additions
      );

      store.add(
        $rdf.sym(oldFileLocation.value),
        NIE("dataSource"),
        fileService.value,
        graphs.additions
      );
    }
  },
};

async function getUploadResources(uri, mu, sudo) {
  const { results } = await sudo.querySudo(`
      PREFIX lblodSubsidie: <http://lblod.data.gift/vocabularies/subsidie/>
      PREFIX schema: <http://schema.org/>
      PREFIX nie: <http://www.semanticdesktop.org/ontologies/2007/01/19/nie#>
      PREFIX foaf: <http://xmlns.com/foaf/0.1/>
      SELECT DISTINCT ?firstName ?familyName ?email ?telephone
      WHERE {
        GRAPH ?g {
          ${mu.sparqlEscapeUri(uri)}
            schema:contactPoint ?contactPoint.
          OPTIONAL {
              ?contactPoint foaf:familyName ?familyName.
          }
          OPTIONAL {
              ?contactPoint foaf:firstName ?firstName.
          }
          OPTIONAL {
              ?contactPoint schema:email ?email.
          }
          OPTIONAL {
              ?contactPoint schema:telephone ?telephone.
          }
        }
      }`);

  if (results.bindings.length) {
    return results.bindings[0];
  }
  return null;
}

async function getFileResources() {
  return null;
}

function duplicateFile() {
  return null;
}
