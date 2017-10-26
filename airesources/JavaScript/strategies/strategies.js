const constants = require('../hlt/Constants');
const Geometry = require('../hlt/Geometry');

/**
 * strategy is a function that accepts the current game map and return a list of next steps to take
 * @param {GameMap} gameMap
 * @returns {string[]} moves that needs to be taken. null values are ignored
 */
function defaultStrategy(gameMap) {
    // Here we build the set of commands to be sent to the Halite engine at the end of the turn
    // one ship - one command
    // in this particular strategy we only give new commands to ships that are not docked
    const moves = gameMap.myShips
        .filter(s => s.isUndocked())
        .map(ship => {
            // find the planets that are free or occupied by you
            const planetsOfInterest = gameMap.planets.filter(p => p.isFree() ||
                (p.isOwnedByMe() && p.hasDockingSpot() ));

            if (planetsOfInterest.length === 0) {
                return null; // if all the planets are taken we return null - no move for this ship
            }

            // sorting planets based on the distance to the ship
            const sortedPlants = [...planetsOfInterest].sort((a, b) => Geometry.distance(ship, a) - Geometry.distance(ship, b));
            const chosenPlanet = sortedPlants[0];

            if (ship.canDock(chosenPlanet)) {
                return ship.dock(chosenPlanet);
            } else {
                /*
                 If we can't dock, we approach the planet with constant speed.
                 Don't worry about pathfinding for now, as the command will do it for you.
                 We run this navigate command each turn until we arrive to get the latest move.
                 Here we move at half our maximum speed to better control the ships.
                 Navigate command is an example and most likely you will have to design your own.
                 */
                return ship.navigate({
                    target: chosenPlanet,
                    keepDistanceToTarget: chosenPlanet.radius + 3,
                    speed: constants.MAX_SPEED / 2,
                    avoidObstacles: true,
                    ignoreShips: false
                });
            }
        });

    return moves; // return moves assigned to our ships for the Halite engine to take
}

function westernDuel(gameMap) {
    const ship = gameMap.myShips[0];
    const thrustMove = ship.navigate({
        target: ship.pointApproaching({x: 0, y: 0}, 4),
        speed: constants.MAX_SPEED / 2,
        avoidObstacles: true,
        ignoreShips: true
    });

    return [thrustMove];
}

module.exports = {defaultStrategy, westernDuel};