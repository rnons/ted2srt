const webpack = require("webpack");

module.exports = {
  context: __dirname,
  devtool: "source-map",
  target: "web",
  entry: {
    Home: ["./src/HomePage.ts"]
  },
  output: {
    path: __dirname + "/../backend/assets",
    filename: "[name].js",
    publicPath: "/"
  },
  resolve: {
    extensions: [".js"]
  },
  module: {
    rules: [
      {
        test: /\.ts$/,
        use: ["ts-loader"]
      },
      {
        test: /\.css$/,
        use: [
          "style-loader",
          {
            loader: "css-loader",
            options: {
              importLoaders: 1,
              modules: true,
              localIdentName: "[name]_[local]_[hash:base64:5]"
            }
          },
          "postcss-loader"
        ]
      }
    ]
  },
  plugins: [],
  devServer: {
    host: "0.0.0.0",
    historyApiFallback: true,
    proxy: {
      "/api": {
        target: "http://localhost:3001",
        secure: false,
        pathRewrite: {
          "^/api": ""
        }
      }
    }
  }
};
