object Env {
  def readId(): Int = {
    read().toInt
  }

  def readInit(): Grid = {
    val dims = read().split(" ")
    val width = dims(0).toInt
    val height = dims(1).toInt

    val locations = Array.ofDim[Location](height, width)
    val productions = read().split(" ")

    for (y <- 0 until height) {
      for (x <- 0 until width) {
        locations(y)(x) = Location(x, y, productions(y * width + x).toInt)
      }
    }

    val occupants = readFrame(width, height)
    new Grid(width, height, locations, occupants)
  }

  def readFrame(width: Int, height: Int): Array[Array[Occupant]] = {
    val ownersStrengths = read().split(" ")
    val strengthsIndex = ownersStrengths.length - width * height

    var y, x, count = 0
    var owner, cur, i = 0

    val occupants = Array.ofDim[Occupant](height, width)
    while (y < height) {
      if (cur < count) {
        occupants(y)(x) = Occupant(owner, ownersStrengths(strengthsIndex + y * width + x).toInt)
        if (x == width - 1) {
          x = 0
          y += 1
        } else {
          x += 1
        }
        cur += 1
      } else {
        count = ownersStrengths(i).toInt
        owner = ownersStrengths(i + 1).toInt
        cur = 0
        i += 2
      }
    }

    occupants
  }

  def writeFrame(moves: Iterable[Move]): Unit = {
    val builder = new StringBuilder()
    moves foreach { m =>
      builder.append(m.location.x)
      builder.append(" ")
      builder.append(m.location.y)
      builder.append(" ")
      builder.append(m.direction.getValue)
      builder.append(" ")
    }
    writeString(builder.toString())
  }

  def writeInit(name: String): Unit = {
    writeString(name)
  }

  private def writeString(s: String): Unit = {
    System.out.print(s + '\n')
    System.out.flush()
  }

  private def read(): String = {
    val buffer = new StringBuilder()
    var next = System.in.read()
    while (next != '\n' && next != 0) {
      buffer.append(next.asInstanceOf[Char])
      next = System.in.read()
    }
    buffer.toString
  }
}
