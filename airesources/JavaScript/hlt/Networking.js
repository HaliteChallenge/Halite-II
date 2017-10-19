const readline = require('readline');

const rl = readline.createInterface({
    input: process.stdin,
    output: process.stdout
});

class Networking {
    static writeLine(line) {
        console.log(line)
    }

    static sendMoves(moves) {
        Networking.writeLine(moves.join(' '));
    }

    static readLine(onLineCallback) {
        rl.question('', (line) => {
            onLineCallback(line)
        });
    }

    static forEachReadLine(onLineCallback) {
        rl.on('line', (line) => {
            onLineCallback(line);
        })
    }
}

module.exports = Networking;
