const readline = require('readline');

const rl = readline.createInterface({
    input: process.stdin,
    output: process.stdout
});

class Networking {
    static writeLine(line) {
        console.log(line);
    }

    static sendMoves(moves) {
        Networking.writeLine(moves.join(' ') + '\n');
    }

    static readNLines(n, onLines) {
        const lines = [];

        readLine();

        function readLine() {
            rl.question('', onLine);
        }

        function onLine(line) {
            lines.push(line);
            if (lines.length === n) {
                onLines(lines);
            } else {
                readLine();
            }
        }
    }

    static forEachReadLine(onLineCallback) {
        rl.on('line', (line) => {
            onLineCallback(line);
        })
    }
}

module.exports = Networking;
