import halite.*


fun main(args: Array<String>) {
    val networking = Networking()
    val gameMap = networking.initialize("Ender")

    val moveList = mutableListOf<Move>()
    while (true) {
        moveList.clear()
        gameMap.updateMap(Networking.readLineIntoMetadata())

        gameMap.myPlayer.ships.values
                .filter { it.dockingStatus == DockingStatus.Undocked }
                .forEach { ship ->
                    val planet = gameMap.allPlanets.values.firstOrNull { !it.isOwned }
                    if (planet != null) {
                        if (ship.canDock(planet)) {
                            moveList.add(DockMove(ship, planet))
                        } else {
                            val newThrustMove = Navigation(ship, planet).navigateToDock(gameMap, Constants.MAX_SPEED / 2)

                            if (newThrustMove != null) {
                                moveList.add(newThrustMove)
                            }
                        }
                    }
                }
        Networking.sendMoves(moveList)
    }
}

