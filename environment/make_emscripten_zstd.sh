#!/bin/sh

# Script to build JavaScript/Emscripten version of libzstd

cd ./zstd-1.3.0/lib
emmake make SHARED_EXT='bc' lib-release
cd ..
emcc shim.c -o shim.bc
emcc ./lib/libzstd.bc ./shim.bc -O2 --memory-init-file 0 \
     -s 'EXPORT_NAME="libzstd"' \
     -s 'EXPORTED_FUNCTIONS=["_ZSTD_versionNumber", "_ZSTD_decompress", "_ZSTD_getFrameContentSize", "_ZSTD_isError", "_ZSTD_DStreamInSize", "_ZSTD_DStreamOutSize", "_ZSTD_createDStream", "_ZSTD_initDStream", "_ZSTD_decompressStream", "_ZSTD_freeDStream", "_ZSTDshim_makeInBuffer", "_ZSTDshim_makeOutBuffer", "_ZSTDshim_inBufferExhausted", "_ZSTDshim_outBufferPos", "_ZSTD_getErrorName", "_ZSTDshim_decompress"]' \
     -s 'MODULARIZE=1' -s 'ALLOW_MEMORY_GROWTH=1' -o ../libzstd.js
