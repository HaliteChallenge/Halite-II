const path = require("path");

module.exports = {
    entry: "./index.js",
    output: {
        path: path.resolve(__dirname, "dist"),
        filename: "bundle.js",
    },
    devtool: "source-map",
    module: {
        loaders: [
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
};