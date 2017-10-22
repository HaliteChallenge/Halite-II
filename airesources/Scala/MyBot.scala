/**
  * Welcome to your first Halite-II bot!
  *
  * This bot's name is Settler. It's purpose is simple (don't expect it to win complex games :) ):
  *     1. Initialize game
  *     2. If a ship is not docked and there are unowned planets
  *     2.a. Try to Dock in the planet if close enough
  *     2.b If not, go towards the planet
  *
  * Note: Please do not place print statements here as they are used to communicate with the Halite engine. If you need
  * to log anything use the logging module.
  */
// Let's start by importing some classes from the Halite Starter Kit so we can interface with the Halite engine
import hlt.{Constants, DockMove, Move, Navigation, Networking, Planet, Ship}

object MyBot {
  def main(args: Array[String]): Unit = {
    // GAME START

    // Initialize the communication layer with the Halite engine
    val networking = new Networking("Johnny5")
    // Get the initial game map. We don't do anything with it in this bot, but more advanced
    // robots may take advantage of it
    val initialGameMap = networking.nextGameMap()

    while (true) {
      // TURN START
      // Update the map for the new turn and get the latest version
      val gameMap = networking.nextGameMap()

      // We're only interested our ships that are currently undocked, so we filter them from the list
      val myUndockedShips: Iterable[Ship] = gameMap.getMyPlayer.getShips.values
        .filter(_.dockingStatus == Ship.Undocked)

      /**
        * This function takes a ship, and collection of planets and tries to dock or navigate to
        * the first unowned planet.
        *
        * @param ship The ship to move
        * @param planets The list of all planets
        * @return None if there are no unowned planets
        */
      def getMoveForShip(ship: Ship, planets: Iterable[Planet]): Option[Move] = {
        // We only want to move to unowned planets, so filter out the owned ones.
        val unownedPlanets = planets.filter(!_.isOwned)
        // If all the planets are owned, return None
        if (unownedPlanets.isEmpty) {
          None
        } else {
          // We only consider the first planet
          val firstUnownedPlanet = unownedPlanets.head
          // If we can dock, let's (try to) dock. If two ships try to dock at once, neither will be able to.
          if (ship.canDock(firstUnownedPlanet)) {
            Some(new DockMove(ship, firstUnownedPlanet))
          } else {
            // If we can't dock, we move towards the closest empty point near this planet (by using closest_point_to)
            // with constant speed. Don't worry about pathfinding for now, as the command will do it for you.
            // We run this navigate command each turn until we arrive to get the latest move.
            // Here we move at half our maximum speed to better control the ships
            // In order to execute faster we also choose to ignore ship collision calculations during navigation.
            // This will mean that you have a higher probability of crashing into ships, but it also means you will
            // make move decisions much quicker. As your skill progresses and your moves turn more optimal you may
            // wish to turn that option off.
            new Navigation(ship, firstUnownedPlanet)
            // If the move is possible, this method will return Some(Move). If there are too many
            // obstacles on the way or we are trapped (or we reached our destination!), the method will
            // return None; don't fret though, we can run the command again the next turn)
              .navigateToDock(gameMap, Constants.MAX_SPEED / 2)
          }
        }
      }

      // Send the list of moves to the Halite engine for this turn
      Networking.sendMoves(
        // Map the list of moves for each ship and flatten to remove any None values
        myUndockedShips.flatMap(ship => getMoveForShip(ship, gameMap.getAllPlanets.values)))
    } // TURN END
  } // MAIN END
}
