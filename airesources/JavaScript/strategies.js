/**
 * strategy is a function that accepts the current game map and return a list of next steps to take
 * @param {GameMap} gameMap
 * @returns {Array}
 */
function myStrategy(gameMap) {
    const moves = gameMap.myShips.map(s => s.thrust(1, 0));
    return moves;
}

module.exports = {myStrategy};