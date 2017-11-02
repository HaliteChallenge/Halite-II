const pako = require("pako");
const libzstd = require("./libzstd");

const libzstdInstance = libzstd();

const ZSTD_getFrameContentSize = libzstdInstance.cwrap(
    "ZSTD_getFrameContentSize",
    "number",
    ["number", "number"]
);
const ZSTD_decompress = libzstdInstance.cwrap(
    "ZSTD_decompress",
    "number",
    ["number", "number", "number", "number"]
);
const ZSTD_isError = libzstdInstance.cwrap(
    "ZSTD_isError",
    "number",
    ["number"]
);
const ZSTD_getErrorName = libzstdInstance.cwrap(
    "ZSTD_getErrorName",
    "number",
    ["string"]
);
const ZSTDshim_decompress = libzstdInstance.cwrap(
    "ZSTDshim_decompress",
    "number",
    ["number", "number", "number"]
);


function malloc(size) {
    const buffer = libzstdInstance._malloc(size);
    const view = new Uint8Array(libzstdInstance.HEAPU8.buffer,
                                buffer, size);
    return [buffer, view];
}


addEventListener("message", (e) => {
    const buffer = e.data;
    try {
        const inflated = pako.inflate(buffer);
        console.log("Replay was gzipped");
        self.postMessage(inflated.buffer, [inflated.buffer]);
        return;
    }
    catch (e) {
        console.debug("miniz failed, trying zSTD");
        // Not compressed with miniz, let's try zstd
    }

    try {
        // Copy the data to the Emscripten heap
        // http://kapadia.github.io/emscripten/2013/09/13/emscripten-pointers-and-pointers.html
        const byteView = new Uint8Array(buffer);
        const bufferBytes = byteView.length * byteView.BYTES_PER_ELEMENT;
        const [heapBuffer, heapBufferView] = malloc(bufferBytes);
        heapBufferView.set(new Uint8Array(buffer));

        const [resultSize, resultSizeView] = malloc(4);
        const result = ZSTDshim_decompress(heapBufferView.byteOffset, byteView.length, resultSizeView.byteOffset);
        if (result === 0) {
            // TODO: get error
            console.error("Could not decompress replay.");
            return;
        }
        let size = 0;

        const resultSizeInt = new Uint32Array(libzstdInstance.HEAPU8.buffer, resultSize, 1);
        const newBuffer = libzstdInstance.HEAPU8.buffer.slice(result, result + resultSizeInt[0]);
        self.postMessage(newBuffer, [newBuffer]);
    }
    catch (e) {
        console.debug("zSTD failed: ", e);
        // Not compressed with zSTD, plain data?
    }

    // TODO:
    self.postMessage(buffer, [buffer]);
});
