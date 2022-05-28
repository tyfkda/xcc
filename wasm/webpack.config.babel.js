import path from 'path'

module.exports = {
  mode: 'production',
  entry: {
    main: './src/www/main.ts',
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
      {test: /\.ts$/, include: /src\/www/, exclude: /node_modules/, use: {loader: 'ts-loader'}},
      {test: /\.c$/, include: /src\/www\/examples/, type: 'asset/source'},
    ],
  },
}
