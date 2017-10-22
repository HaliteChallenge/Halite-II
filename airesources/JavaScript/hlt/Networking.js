const readline = require('readline');

const rl = readline.createInterface({
    input: process.stdin,
    output: process.stdout
});

class Networking {
    static write(message) {
        process.stdout.write(message);
    }

    static writeLine(line) {
        console.log(line);
    }

    static sendMoves(moves) {
        moves.forEach(m => Networking.write(m));
        // Networking.writeLine(moves.join('\n'));
        Networking.writeLine('\n');
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
