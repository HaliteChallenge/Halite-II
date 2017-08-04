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

addEventListener("message", (e) => {
    const buffer = e.data;
    try {
        const inflated = pako.inflate(buffer);
        self.postMessage(inflated.buffer, [inflated.buffer]);
        return;
    }
    catch (e) {
        // Not compressed with miniz, let's try zstd
    }

    try {
        // Copy the data to the Emscripten heap
        // http://kapadia.github.io/emscripten/2013/09/13/emscripten-pointers-and-pointers.html
        const byteView = new Uint8Array(buffer);
        const bufferBytes = byteView.length * byteView.BYTES_PER_ELEMENT;
        const heapBuffer = libzstdInstance._malloc(bufferBytes);
        const heapBufferView = new Uint8Array(libzstdInstance.HEAPU8.buffer,
            heapBuffer, bufferBytes);
        heapBufferView.set(new Uint8Array(buffer));

        // Determine what the decompressed size is so we can allocate enough
        // memory
        const decompressedSize = ZSTD_getFrameContentSize(
            heapBufferView.byteOffset, bufferBytes);
        if (decompressedSize > 0) {
            // Allocate the buffer to hold the decompressed data
            const inflatedBuffer = libzstdInstance._malloc(decompressedSize);
            const inflatedBufferView = new Uint8Array(
                libzstdInstance.HEAPU8.buffer,
                inflatedBuffer, decompressedSize);
            const result = ZSTD_decompress(
                inflatedBufferView.byteOffset, decompressedSize,
                heapBufferView.byteOffset, bufferBytes);
            if (!ZSTD_isError(result)) {
                // Copy decompressed data into a new buffer and transfer it
                const newBuffer = inflatedBufferView.buffer.slice(
                    inflatedBufferView.byteOffset,
                    inflatedBufferView.byteOffset + inflatedBufferView.byteLength);
                self.postMessage(newBuffer, [newBuffer]);
            }
        }
    }
    catch (e) {
        // Not compressed with zSTD, plain data?
    }

    // TODO:
    self.postMessage(buffer, [buffer]);
});
