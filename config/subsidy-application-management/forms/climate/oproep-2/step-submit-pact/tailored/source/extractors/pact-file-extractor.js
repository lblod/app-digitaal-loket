const fs = require("fs");
const URI_BASE = "http://data.lblod.info/form-data/nodes/";
const FILE_SERVICE = "http://mu.semte.ch/services/file-service/files/";

module.exports = {
  name: "climate-subsidy/submit-pact/pact-file-extractor",
  execute: async (store, graphs, lib, target, source) => {
    if (!source) source = target;

    const { $rdf, mu, sudo } = lib;
    var XSD = new $rdf.Namespace("http://www.w3.org/2001/XMLSchema#");
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

    const results = await getUploadResources(source.uri, mu, sudo);

    if (results.bindings.length) {
      for (const binding of results.bindings) {
        const fileServiceUuid = mu.uuid();
        const fileService = new $rdf.NamedNode(FILE_SERVICE + fileServiceUuid);

        const {
          oldFileService,
          type,
          created,
          modified,
          format,
          fileName,
          fileSize,
          fileExtension,
        } = binding;

        store.add(
          $rdf.sym(signedPact),
          DCT("hasPart"),
          $rdf.sym(fileService),
          graphs.additions
        );

        store.add(
          $rdf.sym(fileService),
          W3("type"),
          $rdf.sym(type.value),
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
          $rdf.lit(created.value, "", XSD("dateTime")),
          graphs.additions
        );

        store.add(
          $rdf.sym(fileService),
          DCT("modified"),
          $rdf.lit(modified.value, "", XSD("dateTime")),
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
          $rdf.lit(fileSize.value, "", XSD("integer")),
          graphs.additions
        );

        store.add(
          $rdf.sym(fileService),
          DBPEDIA("fileExtension"),
          fileExtension.value,
          graphs.additions
        );

        const { oldFileLocation, oldFileName, oldFileUuid } =
          await getFileResources(oldFileService.value, mu, sudo);

        const newUuid = mu.uuid();
        const newFileName = "share://" + newUuid + "." + fileExtension.value;
        fs.copyFileSync(
          sharedUriToPath(oldFileLocation.value),
          sharedUriToPath(newFileName),
          fs.constants.COPYFILE_EXCL
        );

        store.add(
          $rdf.sym(newFileName),
          W3("type"),
          $rdf.sym(type.value),
          graphs.additions
        );

        store.add(
          $rdf.sym(newFileName),
          CORE("uuid"),
          newUuid,
          graphs.additions
        );

        store.add(
          $rdf.sym(newFileName),
          DCT("created"),
          $rdf.lit(created.value, "", XSD("dateTime")),
          graphs.additions
        );

        store.add(
          $rdf.sym(newFileName),
          DCT("modified"),
          $rdf.lit(modified.value, "", XSD("dateTime")),
          graphs.additions
        );

        store.add(
          $rdf.sym(newFileName),
          DCT("format"),
          format.value,
          graphs.additions
        );

        store.add(
          $rdf.sym(newFileName),
          NFO("fileName"),
          sharedUriToFilename(newFileName),
          graphs.additions
        );

        store.add(
          $rdf.sym(newFileName),
          NFO("fileSize"),
          $rdf.lit(fileSize.value, "", XSD("integer")),
          graphs.additions
        );

        store.add(
          $rdf.sym(newFileName),
          DBPEDIA("fileExtension"),
          fileExtension.value,
          graphs.additions
        );

        store.add(
          $rdf.sym(newFileName),
          NIE("dataSource"),
          $rdf.sym(fileService),
          graphs.additions
        );
      }
    }
  },
};

async function getUploadResources(uri, mu, sudo) {
  const { results } = await sudo.querySudo(`
      PREFIX nie: <http://www.semanticdesktop.org/ontologies/2007/01/19/nie#>
      PREFIX nfo: <http://www.semanticdesktop.org/ontologies/2007/03/22/nfo#>
      PREFIX dbpedia: <http://dbpedia.org/ontology/>
      PREFIX dct: <http://purl.org/dc/terms/>
      PREFIX lblod: <http://lblod.data.gift/vocabularies/subsidie/>

      SELECT DISTINCT ?oldFileService ?type ?created ?modified ?format ?fileName ?fileSize ?fileExtension 
      WHERE {
        GRAPH ?g {
          ${mu.sparqlEscapeUri(uri)}
            lblod:signedPact ?oldNode.
          OPTIONAL {
              ?oldNode dct:hasPart ?oldFileService.
              ?oldFileService a ?type;
                    dct:created ?created;
                    dct:modified ?modified;
                    dct:format ?format;
                    nfo:fileName ?fileName;
                    nfo:fileSize ?fileSize;
                    dbpedia:fileExtension ?fileExtension.
          }
        }
      }`);

  return results;
}

async function getFileResources(uri, mu, sudo) {
  const { results } = await sudo.querySudo(`
    PREFIX nie: <http://www.semanticdesktop.org/ontologies/2007/01/19/nie#>
    PREFIX nfo: <http://www.semanticdesktop.org/ontologies/2007/03/22/nfo#>
    PREFIX dbpedia: <http://dbpedia.org/ontology/>
    PREFIX dct: <http://purl.org/dc/terms/>
    PREFIX lblod: <http://lblod.data.gift/vocabularies/subsidie/>
    PREFIX core: <http://mu.semte.ch/vocabularies/core/>

    SELECT DISTINCT ?oldFileLocation ?oldFileName ?oldFileUuid
    WHERE {
      GRAPH ?g {
          ?oldFileLocation nie:dataSource ${mu.sparqlEscapeUri(uri)}.
          ?oldFileLocation nfo:fileName ?oldFileName;
                  core:uuid ?oldFileUuid.
      }
    }`);

  if (results.bindings.length) {
    return results.bindings[0];
  }
  return null;
}

function sharedUriToPath(uri) {
  return uri.replace("share://", "/share/");
}

function sharedUriToFilename(uri) {
  return uri.replace("share://", "");
}
