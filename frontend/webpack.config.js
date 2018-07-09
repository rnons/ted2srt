const webpack = require("webpack");
const ExtractTextPlugin = require("extract-text-webpack-plugin");

let mode;
let resolveModules;

const rules = [
  {
    test: /\.ts$/,
    use: ["ts-loader"]
  }
];
const plugins = [];

if (process.env.NODE_ENV === "production") {
  mode = "production";

  resolveModules = ["node_modules", "dce-output", "src"];

  rules.push({
    test: /\.css$/,
    use: ExtractTextPlugin.extract({
      fallback: "style-loader",
      use: [
        {
          loader: "css-loader",
          options: { importLoaders: 1 }
        },
        "postcss-loader"
      ]
    })
  });

  plugins.push(
    new ExtractTextPlugin({
      filename: "[name].css",
      allChunks: true
    })
  );
} else {
  mode = "development";

  resolveModules = ["node_modules", "output", "src"];

  rules.push({
    test: /\.css$/,
    use: [
      "style-loader",
      {
        loader: "css-loader",
        options: { importLoaders: 1 }
      },
      "postcss-loader"
    ]
  });
}

console.warn("------------------------------------------");
console.warn("webpack running in mode: ", mode);
console.warn("------------------------------------------");

module.exports = {
  context: __dirname,
  mode,
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
    modules: resolveModules,
    extensions: [".js", ".ts"]
  },
  module: {
    rules
  },
  plugins,
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
