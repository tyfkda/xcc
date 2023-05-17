import path from 'path'

module.exports = {
  mode: 'production',
  entry: {
    main: './src/wcc/www/main.ts',
    wasi_worker: './src/wcc/www/wasi_worker.ts',
  },
  output: {
    path: path.resolve(__dirname, 'public'),
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
  optimization: {
    usedExports: true,
  },
}
