const readline = require('readline');

class Networking {
    static initialize(botName, onReadLineCallback) {
        console.log(botName);

        const rl = readline.createInterface({
            input: process.stdin,
            output: process.stdout
        });

        rl.on('line', (line) => {
            onReadLineCallback(line);
        })
    }
}

module.exports = Networking;
