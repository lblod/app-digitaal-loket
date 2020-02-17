module.exports = {
  root: true,
  parserOptions: {
    ecmaVersion: 2018,
    sourceType: 'module'
  },
  extends: [
    'eslint:recommended',
  ],
  env: {
    node: true
  },
  rules: {
    semi: 'warn'
  },
};
