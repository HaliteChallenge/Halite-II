const fs = require('fs');

let logFile;

class Log {
    static init(filePath) {
        logFile = fs.createWriteStream(filePath, {flags : 'w'})
    }

    static log(line) {
        logFile.write(line + '\n');
    }
}

module.exports = Log;