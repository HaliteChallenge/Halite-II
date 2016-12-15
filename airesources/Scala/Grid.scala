class Grid(width: Int, height: Int, locations: Array[Array[Location]], var occupants: Array[Array[Site]]) {
  def update(occupants: Array[Array[Site]]) = this.occupants = occupants

  def getOccupants = occupants
  def getLocations = locations
  def getWidth = width
  def getHeight = height

  def getOccupant(x: Int, y: Int): Site = occupants(y)(x)
  def getLocation(x: Int, y: Int): Location = locations(y)(x)

  def getSpaces: IndexedSeq[Space] = {
    for {
      x <- 0 until width
      y <- 0 until height
    } yield Space(getLocation(x, y), getOccupant(x, y))
  }

  def distance(first: Location, second: Location): Int = {
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

  def angle(first: Location, second: Location): Double = {
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

  def neighbor(location: Location, direction: Int): Location = {
    val x = location.x
    val y = location.y
    if (direction == Direction.NORTH) {
      locations(x)(if (y == 0) height - 1 else y - 1)
    } else if (direction == Direction.EAST) {
      locations(if (x == width - 1) 0 else x + 1)(y)
    } else if(direction == Direction.SOUTH) {
      locations(x)(if (y == height - 1) 0 else y + 1)
    } else if (direction == Direction.WEST) {
      locations(if (x == 0) width - 1 else x - 1)(y)
    } else {
      location
    }
  }
}

case class Location(x: Int, y: Int, production: Int)
case class Site(id: Int, strength: Int)
case class Space(location: Location, occupant: Site)
