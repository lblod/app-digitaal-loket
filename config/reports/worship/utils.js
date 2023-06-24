export function formatObjectTerm(term) {
  switch (term.type) {
    case 'uri':
      return `<${term.value}>`;
    case 'literal':
    case 'typed-literal':
      return `"""${term.value}"""${term.datatype ? `^^<${term.datatype}>` : ''}${
        term['xml:lang'] ? `@${term['xml:lang']}` : ''
      }`;
    default:
      return term.value;
  }
}
