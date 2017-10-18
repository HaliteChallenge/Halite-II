const Networking = require('./Networking');
const Map = require('./Map');

class Game {
    static start(botName, frameAction) {
        const map = new Map({width: 100, height: 100, myPlayerId: 1}); // TODO read from output

        Networking.initialize(botName, (readLine) => {
            map.update(readLine);
            const moves = frameAction(map);
            console.log(moves);
        });
    }
}

module.exports = Game;
