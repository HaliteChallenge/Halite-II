#!/bin/sh

# Script to build JavaScript/Emscripten version of libzstd

cd ./zstd-1.3.0/lib
emmake make SHARED_EXT='bc' lib-release
emcc libzstd.bc -O2 --memory-init-file 0 -s 'EXPORT_NAME="libzstd"' -s 'EXPORTED_FUNCTIONS=["_ZSTD_versionNumber", "_ZSTD_decompress", "_ZSTD_getFrameContentSize", "_ZSTD_isError"]' -s 'MODULARIZE=1' -s 'ALLOW_MEMORY_GROWTH=1' -o ../../libzstd.js
