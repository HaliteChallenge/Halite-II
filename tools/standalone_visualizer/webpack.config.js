const path = require("path");
const webpack = require("webpack");
const CopyWebpackPlugin = require('copy-webpack-plugin');

// https://github.com/vuejs-templates/webpack-simple
module.exports = {
    entry: "./visualizer.js",
    output: {
        path: path.resolve(__dirname, "assets/js"),
        filename: "bundle.js",
    },
    module: {
        rules: [
            {
                test: /\.vue$/,
                loader: 'vue-loader',
                options: {
                    loaders: {
                        'scss': 'vue-style-loader!css-loader!sass-loader',
                        'sass': 'vue-style-loader!css-loader!sass-loader?indentedSyntax'
                    }
                }
            },
            // Work around pixi-extra-filter's use of glslify (which is
            // browserify-dependent) to load shaders
            {
                test: path.resolve(__dirname, "node_modules", "pixi-extra-filters"),
                loader: "ify-loader",
            },
            {
                test: /pixi-extra-filters/,
                loader: "ify-loader",
            },
            {
                test: /\.png$/,
                loader: "file-loader",
            },
        ],
    },
    resolve: {
        alias: {
            'vue$': 'vue/dist/vue.esm.js'
        }
    },
    node: {
          fs: 'empty'
    },
    plugins: [
        new webpack.DefinePlugin({
            api_server_url: "'https://api.halite.io/v1/api'",
            login_server_url: "'https://api.halite.io/v1/login'",
            logout_server_url: "'https://api.halite.io/v1/logout/'"
        }),
        new CopyWebpackPlugin([
            { from: '../../website/assets/images/icon-replay.svg', to: 'assets/images/' },
            { from: '../../website/assets/images/loading-icon.gif', to: 'assets/images/' },
            { from: '../../website/assets/images/site_bg.jpg', to: 'assets/images/' },
            { from: '../../website/assets/images/button.png', to: 'assets/images/' },
            { from: '../../website/assets/fonts/*', to: 'assets/',
              context: '../../website/assets/'},
            { from: '../../website/_site/assets/css/main_new.css', to: 'assets/css/',
              transform: function(contents) {
                  return contents.toString().replace(/\/assets/g, "..");
              }},
        ]),
    ],
};
