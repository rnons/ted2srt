const webpack = require('webpack');
const HtmlWebpackPlugin = require('html-webpack-plugin');


module.exports = {
  devtool: 'source-map',
  entry: './src/main.ts',
  output: {
    path: __dirname + '/dist',
    filename: 'main.js'
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
          'css-loader?importLoaders=1&modules&localIdentName=[folder]_[local]_[hash:base64:5]',
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
    new webpack.LoaderOptionsPlugin({
      test: /\.css$/,
      options: {
        postcss() {
          return [
            require('postcss-for'),
            require('postcss-cssnext')
          ];
        }
      }
    })
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
