## Adding Bestuurseenheid viewOnlyModules flag

```
# Adding viewOnlyModules flag to the bestuurseenheid who has access to the eredienst modules

PREFIX besluit: <http://data.vlaanderen.be/ns/besluit#>
PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>

INSERT {
  GRAPH <http://mu.semte.ch/graphs/public> {
     ?bestuurseenheid ext:viewOnlyModules "LoketLB-eredienstBedienaarGebruiker";
     		              ext:viewOnlyModules "LoketLB-eredienstMandaatGebruiker" .
  }
}
WHERE {
  GRAPH <http://mu.semte.ch/graphs/public> {
     VALUES ?bestuurseenheid {
      <http://data.lblod.info/id/besturenVanDeEredienst/b09aa33ec9809bb0ea9099e6fa569d2a>
     }
     ?bestuurseenheid a besluit:Bestuurseenheid .
  }
}
```

**This will not run by default, if needed you will need to run it locally.**