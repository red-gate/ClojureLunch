var ExtractTextPlugin = require('extract-text-webpack-plugin');
var CopyWebpackPlugin = require("copy-webpack-plugin");
var webpack = require('webpack');
module.exports = {
  devtool: 'eval-source-map',

  devServer: {
    contentBase: '.',
    port: 4008,
    stats: 'errors-only'
  },
  entry: ["./web/static/css/app.css", "./web/static/js/app.js"],
  output: {
    path: __dirname + "/priv/static/",
    pathinfo: true,
    filename: "js/app.js"
  },
 module: {
    rules: [
        { test: /\.(js|jsx)?$/, loader: 'babel-loader', exclude: /node_modules/,  options: { presets: [['es2015', { modules: false }]] }},
        { test: /\.css$/, loader: ExtractTextPlugin.extract(
            {
                fallback: "style-loader",
                use: "css-loader"
            }
        )},
        {
          test: /\.purs$/,
          loader: 'purs-loader',
          query: {
            src: [ 'web/static/purescript/**/*.purs' ],
            bundle: false,
            pscIde: false,
            pscPackage: true
          }
        }
    ]},
    plugins: [new webpack.ProvidePlugin({
      "window.Phoenix": "phoenix"
    })
             ,  new ExtractTextPlugin({ filename: 'css/app.css' })
             , new CopyWebpackPlugin([{ from: "./web/static/assets" }])],
    resolve: {
    }
};