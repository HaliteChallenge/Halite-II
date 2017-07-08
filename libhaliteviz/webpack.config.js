const path = require("path");

module.exports = {
    entry: "./src/main.js",
    output: {
        path: path.resolve(__dirname, "dist"),
        filename: "bundle.js",
        library: "libhaliteviz",
    },
    devtool: "source-map",
    module: {
        loaders: [
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
};
