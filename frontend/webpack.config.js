const webpack = require("webpack");

module.exports = {
  context: __dirname,
  devtool: "source-map",
  target: "web",
  entry: {
    Home: ["./src/common.ts", "./src/HomePage.ts"],
    Talk: ["./src/common.ts", "./src/TalkPage.ts"],
    Search: ["./src/common.ts", "./src/SearchPage.ts"]
  },
  output: {
    path: __dirname + "/../backend/dist",
    filename: "[name].js",
    publicPath: "/"
  },
  resolve: {
    modules: ["node_modules", "output", "src"],
    extensions: [".js", ".ts"]
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
            options: { importLoaders: 1 }
          },
          "postcss-loader"
        ]
      }
    ]
  },
  plugins: [],
  optimization: {
    splitChunks: {
      cacheGroups: {
        common: {
          name: "common",
          chunks: "initial",
          minChunks: 3
        }
      }
    }
  },
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
