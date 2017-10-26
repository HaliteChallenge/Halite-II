const readline = require('readline');

const rl = readline.createInterface({
    input: process.stdin,
    output: process.stdout
});

class Networking {
    static writeLine(line) {
        process.stdout.write(line + '\n');
    }

    static sendMoves(moves) {
        Networking.writeLine(moves.join(' '));
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

    static readLine(onLine) {
        Networking.readNLines(1, lines => onLine(lines[0]));
    }

    static forEachReadLine(onLineCallback) {
        rl.on('line', (line) => {
            onLineCallback(line);
        })
    }
}

module.exports = Networking;
