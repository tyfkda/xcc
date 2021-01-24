import path from 'path'

module.exports = {
  mode: 'production',
  entry: {
    lib: './src/www/lib.ts',
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
    ],
  },
  optimization: {
    splitChunks: {
      cacheGroups: {
        lib: {
          test: /[\\/]node_modules[\\/]/,
          name: 'lib',
          enforce: true,
          chunks: 'all',
        },
      },
    },
  },
}
