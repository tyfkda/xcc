module.exports = {
  extends: [
    'eslint:recommended',
    'plugin:@typescript-eslint/eslint-recommended',
    'plugin:@typescript-eslint/recommended',
  ],
  plugins: [
    '@typescript-eslint',
  ],
  env: { node: true, es6: true },
  parser: '@typescript-eslint/parser',
  parserOptions: {
    sourceType: 'module',
    project: './tsconfig.json',
  },
  rules: {
    'nonblock-statement-body-position': ['error', 'below'],
    'quotes': ['error', 'single', { "avoidEscape": true }],
    'semi': ['error', 'never'],

    '@typescript-eslint/no-unused-vars': ['error', { 'varsIgnorePattern': '^_',  'argsIgnorePattern': '^_' }],
    '@typescript-eslint/no-explicit-any': 'off',
    '@typescript-eslint/no-non-null-assertion': 'off'
  },
}
