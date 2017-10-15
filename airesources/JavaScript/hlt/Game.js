const Networking = require('./Networking');

class Game {
    static start(botName, frameAction) {
        Networking.initialize(botName, (readLine) => {
            const moves = frameAction();
            console.log(moves);
        });
    }
}

module.exports = Game;
