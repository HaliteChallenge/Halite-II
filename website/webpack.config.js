const path = require("path");

// https://github.com/vuejs-templates/webpack-simple
module.exports = {
    entry: "./javascript/main.js",
    output: {
        path: path.resolve(__dirname, "assets/js/"),
        filename: "bundle.js",
    },
    devtool: "source-map",
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
        ],
    },
    resolve: {
        alias: {
            'vue$': 'vue/dist/vue.esm.js'
        }
    },
};