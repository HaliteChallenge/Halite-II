const Game = require('./hlt/Game');
const Moves = require('./hlt/Moves');

Game.start('JsBot', (map) => {
    const movesList = [];
    movesList.push(Moves.dock(1, 2));

    return movesList;
});
