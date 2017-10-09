import hlt._

object MyBot {
  def main(args: Array[String]): Unit = {
    val networking = new Networking
    val (width, height, myId) = networking.initialize("Tamagocchi")
    val initialGameMap = new GameMap(width, height, myId, Networking.readAndSplitLine)

    while (true) {
      val gameMap = new GameMap(width, height, myId, Networking.readAndSplitLine)
      val undockedShips: Iterable[Ship] = gameMap.getMyPlayer.getShips.values
        .filter(_.getDockingStatus == Ship.Undocked)

      def getMoveForShip(ship: Ship, unownedPlanets: Iterable[Planet]): Move = {
        if (unownedPlanets.isEmpty)
          null
        else {
          if (ship.canDock(unownedPlanets.head))
            new DockMove(ship, unownedPlanets.head)
          else
            new Navigation(ship, unownedPlanets.head).navigateToDock(gameMap, Constants.MAX_SPEED / 2)
        }
      }

      Networking.sendMoves(undockedShips.map(ship => getMoveForShip(ship, gameMap.getAllPlanets.values.filter(!_.isOwned))).filter(_ != null))
    }
  }
}
