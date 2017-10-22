const Log = require('./Log');

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

        let turnNumber = 1;
        Networking.readNLines(2, lines => {
            const parsedGameMeta = parseGameMeta(lines);

            Log.init(botName + parsedGameMeta.myPlayerId + '.log');

            Log.log('game meta:');
            lines.forEach(line => Log.log(line));
            Log.log(JSON.stringify(parsedGameMeta));

            mapParser = new GameMapParser(parsedGameMeta);

            Networking.forEachReadLine(line => {
                Log.log(`turn #${turnNumber}, map:`);
                Log.log(line);

                const map = mapParser.parse(line);
                const moves = strategy(map);

                Networking.sendMoves(moves.filter(m => m !== null));
                Log.log('moves:');
                Log.log(moves.join(' '));

                turnNumber++;
            })
        });
    }
}

function parseGameMeta(lines) {
    const playerId = parseInt(lines[0]);
    const widthHeight = lines[1].trim().split(' ');
    return {
        myPlayerId: playerId,
        width: parseInt(widthHeight[0]),
        height: parseInt(widthHeight[1])
    }
}


module.exports = Game;
