const webpack = require('webpack');
const HtmlWebpackPlugin = require('html-webpack-plugin');


module.exports = {
  context: __dirname,
  devtool: 'source-map',
  target: 'web',
  entry: './src/main.ts',
  output: {
    path: __dirname + '/dist',
    filename: 'main.js',
    publicPath: '/'
  },
  resolve: {
    extensions: ['.ts', '.js']
  },
  module: {
    rules: [
      {
        test: /\.css$/,
        use: [
          'style-loader',
          {
            loader: 'css-loader',
            options: {
              importLoaders: 1,
              modules: true,
              localIdentName: '[name]_[local]_[hash:base64:5]',
            }
          },
          'postcss-loader',
        ]
      }, {
        test: /\.ts$/,
        use: [
          'awesome-typescript-loader',
        ]
      }
    ]
  },
  plugins: [
    new HtmlWebpackPlugin({
      template: 'src/index.html',
      filename: 'index.html',
      favicon: 'assets/favicon.ico'
    }),
  ],
  devServer: {
    proxy: {
      '/api': {
        target: 'http://localhost:3001',
        secure: false,
        pathRewrite: {
          '^/api' : ''
        }
      }
    }
  }
};
