const Networking = require('./Networking');
const MapParser = require('./MapParser');

let mapParser = null;

class Game {
    static start(botName, frameAction) {
        Networking.writeLine(botName);

        Networking.readLine(line => {
            mapParser = new MapParser(extractGameMeta(line));

            Networking.forEachReadLine(line => {
                const map = mapParser.parse(line);
                const moves = frameAction(map);
                Networking.sendMoves(moves);
            })
        });
    }
}

function extractGameMeta(line) {
    const tokens = line.trim().split(' ');
    return {
        myPlayerId: parseInt(tokens[0]),
        width: parseInt(tokens[1]),
        height: parseInt(tokens[2])
    }
}


module.exports = Game;
