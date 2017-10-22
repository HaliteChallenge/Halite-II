package hlt

object GameMap {
  private def addEntitiesBetween(start: Position,
                                 target: Position,
                                 entitiesToCheck: Iterable[Entity]): Iterable[Entity] = {
    entitiesToCheck
      .filter(_ != start)
      .filter(_ != target)
      .filter(entity =>
        Collision.segmentCircleIntersect(start, target, entity, Constants.FORECAST_FUDGE_FACTOR))
  }
}

class GameMap(val width: Short,
              val height: Short,
              val playerId: Short,
              val metadataIterator: Iterator[String]) {
  DebugLog.addLog("--- NEW TURN ---")
  var players: Seq[Player] = {
    val numberOfPlayers = MetadataParser.parsePlayerNum(metadataIterator)

    // update players info
    def getPlayerFromMetadata: Player = {
      val playerId = MetadataParser.parsePlayerId(metadataIterator)
      val ships = MetadataParser.getShipList(Some(playerId), metadataIterator)
      new Player(playerId, ships.map(ships => ships.id -> ships).toMap)
    }

    for (counter <- 1 to numberOfPlayers) yield getPlayerFromMetadata
  }
  DebugLog.addLog("Players: " + players)

  var planets: Map[Long, Planet] = {
    val numberOfPlanets = metadataIterator.next.toLong
    (for (counter <- 1 to numberOfPlanets.toInt)
      yield MetadataParser.newPlanetFromMetadata(metadataIterator))
      .map(planet => planet.id -> planet)
      .toMap
  }
  DebugLog.addLog("Planets: " + planets)

  if (!metadataIterator.isEmpty) {
    throw new IllegalStateException(
      "Failed to parse data from Halite game engine. Please contact maintainers.")
  }

  var allShips: Seq[Ship] = {
    def getShipsFromPlayers(players: Seq[Player], shipAccum: Seq[Ship]): Seq[Ship] = {
      if (players.isEmpty) {
        shipAccum
      } else {
        getShipsFromPlayers(players.tail, players.head.getShips.values.toSeq ++ shipAccum)
      }
    }

    getShipsFromPlayers(players, Seq.empty)
  }
  DebugLog.addLog("All ships: " + allShips)

  def getAllPlayers: Seq[Player] = players

  def getMyPlayer: Player = players(getMyPlayerId)

  def getMyPlayerId: Short = playerId

  @throws[IndexOutOfBoundsException]
  def getShip(playerId: Short, entityId: Long): Option[Ship] =
    players(playerId).getShip(entityId)

  def getPlanet(entityId: Long): Planet = planets(entityId)

  def getAllPlanets: Map[Long, Planet] = planets

  def getAllShips: Seq[Ship] = allShips

  def objectsBetween(start: Position, target: Position): Iterable[Entity] = {
    GameMap.addEntitiesBetween(start, target, planets.values) ++ GameMap
      .addEntitiesBetween(start, target, allShips)
  }

  def nearbyEntitiesByDistance(entity: Entity): Map[Double, Entity] = {
    (planets.values
      .filter(_ != entity)
      .map(planet => entity.getDistanceTo(planet) -> planet)
      .toMap[Double, Entity]
      ++
        allShips
          .filter(_ != entity)
          .map(planet => entity.getDistanceTo(planet) -> planet)
          .toMap[Double, Entity])
  }
}
