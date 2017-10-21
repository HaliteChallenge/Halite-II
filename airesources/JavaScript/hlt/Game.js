const Networking = require('./Networking');
const GameMapParser = require('./GameMapParser');

let mapParser = null;

class Game {
    /**
     * starts a game with a specified bot name and a strategy
     * @param botName bot name
     * @param {function} strategy function with game map as a parameter that returns a list of moves to take
     */
    static start(botName, strategy) {
        Networking.writeLine(botName);

        Networking.readLine(line => {
            mapParser = new GameMapParser(extractGameMeta(line));

            Networking.forEachReadLine(line => {
                const map = mapParser.parse(line);
                const moves = strategy(map);
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
