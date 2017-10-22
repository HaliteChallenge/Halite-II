package hlt

object MetadataParser {
  private def getShort(metadata: Iterator[String]) = metadata.next.toShort

  private def getInt(metadata: Iterator[String]) = metadata.next.toInt

  private def getLong(metadata: Iterator[String]) = metadata.next.toLong

  private def getDouble(metadata: Iterator[String]) = metadata.next.toDouble

  def getShipList(owner: Option[Short], shipsMetadata: Iterator[String]): Seq[Ship] = {
    val numberOfShips = getLong(shipsMetadata)
    for (i <- 1 to numberOfShips.toShort)
      yield newShipFromMetadata(owner, shipsMetadata)
  }

  private def newShipFromMetadata(owner: Option[Short], metadata: Iterator[String]) = {
    val id = getLong(metadata)
    val xPos = getDouble(metadata)
    val yPos = getDouble(metadata)
    val health = getShort(metadata)
    // Ignoring velocity(x,y) which is always (0,0) in current version.
    metadata.next()
    metadata.next()
    val dockingStatus = Ship.values(getShort(metadata))
    val dockedPlanet = getLong(metadata)
    val dockingProgress = getShort(metadata)
    val weaponCooldown = getShort(metadata)
    new Ship(owner,
             id,
             xPos,
             yPos,
             health,
             dockingStatus,
             dockedPlanet,
             dockingProgress,
             weaponCooldown)
  }

  def newPlanetFromMetadata(metadata: Iterator[String]): Planet = {
    val id = getLong(metadata)
    val xPos = getDouble(metadata)
    val yPos = getDouble(metadata)
    val health = getShort(metadata)
    val radius = getDouble(metadata)
    val dockingSpots = getShort(metadata)
    val currentProduction = getShort(metadata)
    val remainingProduction = getShort(metadata)
    var owner: Option[Short] = Some(0)
    if (getInt(metadata) == 1) {
      owner = Some(getShort(metadata))
    } else {
      owner = None
      metadata.next
    }
    val dockedShipCount = getInt(metadata)
    val dockedShips = for (i <- 1 to dockedShipCount) yield getLong(metadata)
    new Planet(owner,
               id,
               xPos,
               yPos,
               health,
               radius,
               dockingSpots,
               currentProduction,
               remainingProduction,
               dockedShips)
  }

  def parsePlayerNum(metadata: Iterator[String]): Short = getShort(metadata)

  def parsePlayerId(metadata: Iterator[String]): Short = getShort(metadata)
}
