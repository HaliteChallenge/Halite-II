object Grid {
  val NEUTRAL = 0
}

class Grid(width: Int, height: Int, locations: Array[Array[Location]], var occupants: Array[Array[Occupant]]) {
  def update(occupants: Array[Array[Occupant]]) = this.occupants = occupants

  def getWidth = width
  def getHeight = height

  def getOccupant(x: Int, y: Int): Occupant = occupants(y)(x)
  def getLocation(x: Int, y: Int): Location = locations(y)(x)
  def getSite(x: Int, y: Int): Site = Site(getLocation(x, y), getOccupant(x, y))

  def getSites: Seq[Site] = {
    for {
      x <- 0 until width
      y <- 0 until height
    } yield getSite(x, y)
  }

  def getMine(id: Int): Seq[Site] = {
    for {
      x <- 0 until width
      y <- 0 until height
      if getOccupant(x, y).id == id
    } yield getSite(x, y)
  }

  def getDistance(first: Location, second: Location): Int = {
    var dx = Math.abs(first.x - second.x)
    var dy = Math.abs(first.y - second.y)

    if (dx > width / 2.0) {
      dx = width - dx
    }

    if (dy > height / 2.0) {
      dy = height - dy
    }

    dx + dy
  }

  def getAngle(first: Location, second: Location): Double = {
    var dx = first.x - second.x

    // Flip order because 0,0 is top left
    // and want atan2 to look as it would on the unit circle
    var dy = second.y - first.y

    if (dx > width - dx) {
      dx -= width
    }
    if (-dx > width + dx) {
      dx += width
    }

    if (dy > height - dy) {
      dy -= height
    }
    if (-dy > height + dy) {
      dy += height
    }

    Math.atan2(dy, dx)
  }

  def getNeighbor(location: Location, direction: Direction): Neighbor = {
    val x = location.x
    val y = location.y
    direction match {
      case North => Neighbor(getSite(x, if (y == 0) height - 1 else y - 1), North)
      case East => Neighbor(getSite(if (x == width - 1) 0 else x + 1, y), East)
      case South => Neighbor(getSite(x, if (y == height - 1) 0 else y + 1), South)
      case West => Neighbor(getSite(if (x == 0) width - 1 else x - 1, y), West)
      case Still => Neighbor(getSite(x, y), Still)
    }
  }

  def getNeighbors(location: Location): Seq[Neighbor] = {
    Direction.CARDINALS map (d => getNeighbor(location, d))
  }

  def getMyNeighbors(id: Int, location: Location): Seq[Neighbor] = {
    getNeighbors(location).filter(_.site.occupant.id == id)
  }
}

case class Location(x: Int, y: Int, production: Int)
case class Occupant(id: Int, strength: Int)
case class Site(location: Location, occupant: Occupant)
case class Neighbor(site: Site, direction: Direction)
