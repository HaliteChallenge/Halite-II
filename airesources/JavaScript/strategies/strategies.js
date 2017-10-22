const constants = require('../hlt/Constants');

/**
 * strategy is a function that accepts the current game map and return a list of next steps to take
 * @param {GameMap} gameMap
 * @returns {string[]} moves that needs to be taken. null values are ignored
 */
function myStrategy(gameMap) {
    // Here we define the set of commands to be sent to the Halite engine at the end of the turn
    const moves = [];

    gameMap.myShips
        .filter(s => s.isUndocked()) // skip the ships that are docked, docking, or undocking
        .forEach(ship => {

            const emptyPlanets = gameMap.planets.filter(p => p.isFree()); // skip the planets that are occupied

            if (!emptyPlanets.length) {
                return;
            }

            const firstAvailablePlanet = emptyPlanets[0];

            // If we can dock, let's (try to) dock. If two ships try to dock at once, neither will be able to.
            if (ship.canDock(firstAvailablePlanet)) {
                moves.push(ship.dock(planet));
            } else {
                /*
                 If we can't dock, we approach the planet with constant speed.
                 Don't worry about pathfinding for now, as the command will do it for you.
                 We run this navigate command each turn until we arrive to get the latest move.
                 Here we move at half our maximum speed to better control the ships
                 In order to execute faster we also choose to ignore ship collision calculations during navigation.
                 This will mean that you have a higher probability of crashing into ships, but it also means you will
                 make move decisions much quicker. As your skill progresses and your moves turn more optimal you may
                 wish to turn that option off.
                 */

                const pointCloseToPlanet = ship.pointApproaching(firstAvailablePlanet, 3);

                const thrustMove = ship.navigate({
                    target: {id: firstAvailablePlanet.id, x: pointCloseToPlanet.x, y: pointCloseToPlanet.y},
                    speed: constants.MAX_SPEED / 2,
                    avoidObstacles: true, ignoreShips: true
                });

                moves.push(thrustMove);
            }
        });

    return moves; // return moves assigned to our ships for the Halite engine to take
}

module.exports = {myStrategy};