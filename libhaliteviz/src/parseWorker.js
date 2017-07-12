const pako = require("pako");

addEventListener("message", (e) => {
    const buffer = e.data;
    const inflated = pako.inflate(buffer);
    self.postMessage(inflated.buffer, [inflated.buffer]);
});
