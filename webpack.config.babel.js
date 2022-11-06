import path from 'path'

module.exports = {
  mode: 'production',
  entry: {
    main: './src/wcc/www/main.ts',
  },
  output: {
    path: path.resolve(__dirname, 'public/assets'),
    filename: '[name].js',
    sourceMapFilename: '[name].map',
  },
  resolve: {
    extensions: ['.ts', '.js'],
  },
  module: {
    rules: [
      {test: /\.ts$/, include: /src\/wcc\/www/, exclude: /node_modules/, use: {loader: 'ts-loader'}},
      {test: /\.c$/, include: /src\/wcc\/www\/examples/, type: 'asset/source'},
    ],
  },
}
