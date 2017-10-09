package hlt

object MetadataParser {
  def getShipList(owner: Option[Short], shipsMetadata: Iterator[String]): Seq[Ship] = {
    val numberOfShips = shipsMetadata.next.toLong
    var i = 0
    for (i <- 1 to numberOfShips.toShort) yield newShipFromMetadata(owner, shipsMetadata)
  }

  private def newShipFromMetadata(owner: Option[Short], metadata: Iterator[String]) = {
    val id = metadata.next.toLong
    val xPos = metadata.next.toDouble
    val yPos = metadata.next.toDouble
    val health = metadata.next.toShort
    // Ignoring velocity(x,y) which is always (0,0) in current version.
    metadata.next()
    metadata.next()
    val dockingStatus = Ship.Values(metadata.next.toShort)
    val dockedPlanet = metadata.next.toLong
    val dockingProgress = metadata.next.toShort
    val weaponCooldown = metadata.next.toShort
    new Ship(owner, id, xPos, yPos, health, dockingStatus, dockedPlanet, dockingProgress, weaponCooldown)
  }

  def newPlanetFromMetadata(metadata: Iterator[String]): Planet = {
    val id = metadata.next.toLong
    val xPos = metadata.next.toDouble
    val yPos = metadata.next.toDouble
    val health = metadata.next.toShort
    val radius = metadata.next.toDouble
    val dockingSpots = metadata.next.toShort
    val currentProduction = metadata.next.toShort
    val remainingProduction = metadata.next.toShort
    var owner: Option[Short] = Some(0)
    if (metadata.next.toInt == 1) {
      owner = Some(metadata.next.toShort)
    }
    else {
      owner = None
      metadata.next
    }
    val dockedShipCount = metadata.next.toInt
    // var i = 0
    val dockedShips = for (i <- 1 to dockedShipCount) yield metadata.next.toLong
    new Planet(owner, id, xPos, yPos, health, radius, dockingSpots, currentProduction, remainingProduction, dockedShips)
  }

  def parsePlayerNum(metadata: Iterator[String]): Short = metadata.next.toShort

  def parsePlayerId(metadata: Iterator[String]): Short = metadata.next.toShort
}