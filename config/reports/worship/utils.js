/**
 * Prints a term in the format of SPARQL JSON results to a string that can be
 * used in RDF output.
 *
 * @public
 * @function
 * @param {Object} term - JSON formatted term that.
 * @return {String} RDF output for that term.
 */
export function formatTerm(term) {
  switch (term.type) {
    case 'uri':
      return `<${term.value}>`;
    case 'literal':
    case 'typed-literal':
      return `"""${term.value}"""${
        term.datatype ? `^^<${term.datatype}>` : ''
      }${term['xml:lang'] ? `@${term['xml:lang']}` : ''}`;
    default:
      return term.value;
  }
}

export function quadsToColumnarCSV(rdfData) {}
