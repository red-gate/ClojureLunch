var ExtractTextPlugin = require('extract-text-webpack-plugin');

module.exports = {
  entry: ["./web/static/css/app.css", "./web/static/js/app.js"],
  output: {
    path: __dirname + "/priv/static/",
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
        )}
    ]},
    plugins: [new ExtractTextPlugin({ filename: 'css/app.css' })]
};