const parseWorker = require("worker-loader?inline!./parseWorker");

export function parseReplay(buffer) {
    return new Promise((resolve, reject) => {
        try {
            const startTime = Date.now();
            const worker = new parseWorker();
            worker.onmessage = function (e) {
                const inflated = e.data;
                const inflatedTime = Date.now();
                const decoded = new TextDecoder("utf-8").decode(new Uint8Array(inflated));
                const replay = JSON.parse(decoded);
                const finishTime = Date.now();
                console.info(`Decoded compressed replay in ${finishTime - startTime}ms, inflating took ${inflatedTime - startTime}ms, decoding took ${finishTime - inflatedTime}ms.`);
                resolve(replay);
            };
            worker.postMessage(buffer, [buffer]);
            if (buffer.byteLength) {
                console.warn("Transferrables not supported, could not decode without copying data!");
            }
        }
        catch (e) {
            console.error(e);
            resolve(msgpack.decode(buffer));
        }
    });
}