/**
 * strategy is a function that accepts the current game map and return a list of next steps to take
 * @param {GameMap} gameMap
 * @returns {Array}
 */
function myStrategy(gameMap) {
    const moves = [];
    moves.push(gameMap.myShips[0].thrust(10, 50));

    return moves;
}

module.exports = {myStrategy};