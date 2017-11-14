package hlt

class Position(val xPos: Double, val yPos: Double) {
  def getDistanceTo(target: Position): Double = {
    val dx = xPos - target.getXPos
    val dy = yPos - target.getYPos
    Math.sqrt(Math.pow(dx, 2) + Math.pow(dy, 2))
  }

  def orientTowardsInDeg(target: Position): Int = Util.angleRadToDegClipped(orientTowardsInRad(target))

  def getClosestPoint(target: Entity): Position = {
    val MIN_DISTANCE = 3
    val radius = target.radius + MIN_DISTANCE
    val angleRad = target.orientTowardsInRad(this)
    val dx = target.getXPos + radius * Math.cos(angleRad)
    val dy = target.getYPos + radius * Math.sin(angleRad)
    new Position(dx, dy)
  }

  def getYPos: Double = yPos

  def orientTowardsInRad(target: Position): Double = {
    val dx = target.getXPos - xPos
    val dy = target.getYPos - yPos
    Math.atan2(dy, dx) + 2 * Math.PI
  }

  def getXPos: Double = xPos

  override def toString: String = "Position(" + xPos + ", " + yPos + ")"

  override def equals(other: Any): Boolean = other match {
    case that: Position =>
      (that canEqual this) &&
        xPos == that.xPos &&
        yPos == that.yPos
    case _ => false
  }

  def canEqual(other: Any): Boolean = other.isInstanceOf[Position]

  override def hashCode(): Int = {
    val state = Seq(xPos, yPos)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}