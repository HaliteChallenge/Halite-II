const merge = require('webpack-merge');
const commonConfig = require('./webpack.common.js');
const BabiliPlugin = require('babili-webpack-plugin');
const webpack = require("webpack");

module.exports = merge(commonConfig, {
    plugins: [
        new webpack.LoaderOptionsPlugin({
            minimize: true,
            debug: false
        }),
        new webpack.DefinePlugin({
            'process.env': {
                'NODE_ENV': JSON.stringify('production')
            }
        }),
        new BabiliPlugin(),
    ]
});
