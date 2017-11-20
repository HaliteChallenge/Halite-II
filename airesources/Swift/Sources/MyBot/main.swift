/*
Welcome to your first Halite-II bot!

This bot's name is Swifty. It's purpose is simple (don't expect it to win complex games :) ):
1. Initialize game
2. If a ship is not docked and there are unowned planets
2.a. Try to Dock in the planet if close enough
2.b If not, go towards the planet

Note: Please do not place print statements here as they are used to communicate with the Halite engine. If you need
to log anything use game.log.
*/

import hlt

// GAME START
// Here we define the bot's name as Swifty and initialize the game, including communication with the Halite engine
let game = Game(botName: "Swifty")

// Then we print our start message to the logs
game.log.info("Starting my Swifty bot!")

// We have 1 full minute to analyse the initial map.
// When the next 'updateMap()' command is run, the game will start.

let initialMapIntellegence =
    "width: \(game.map.width); " +
    "height: \(game.map.height); " +
    "players: \(game.map.players.count); " +
    "planets: \(game.map.planets.count)"

// Log some info about the game map
game.log.info(initialMapIntellegence)

while true {
    // TURN START
    // Update the map for the new turn and get the latest version
    let map = game.updateMap()
    
    // Create a list to hold the set of moves to be sent to the Halite engine at the end of the turn
    var moveList = [Move]()
    
    // For every ship that I control
    for ship in map.getMe().ships {
        
        // If the ship is docked
        if ship.dockingStatus != .undocked {
            // Skip this ship
            continue
        }
        
        // For each planet on the game (only non-destroyed planets are included)
        for planet in map.planets {
            
            // If the planet is owned
            if planet.owned {
                // Skip this planet
                continue
            }
            
            // If we can dock, lets (try to) dock. If two ships try to dock at once, neither will be able to.
            if ship.canDock(withPlanet: planet) {
                // We add the move by appending it to the move list
                moveList.append(.dock(ship, planet: planet))
            } else {
                /*
                 If we can't dock, we move towards the closest empty point near this planet (by using getClosestPoint(to:))
                 with constant speed. Don't worry about pathfinding for now, as the command will do it for you.
                 We run this navigate command each turn until we arrive to get the latest move.
                 Here we move at half our maximum speed to better control the ships.
                 */
                
                let navigateMove = ship.navigate(towards: ship.getClosestPoint(to: planet),
                                                    map: map,
                                                    maxThrust: Constants.MaxSpeed / 2,
                                                    avoidObstacles: true)
                
                moveList.append(navigateMove)
            }
            
            break
        }
    }
    
    // Send our list of commands to the Halite engine for this turn
    game.sendMoves(moveList)
    // TURN END
}
// GAME END

