package hlt

object Collision {

  /**
    * Test whether a given line segment intersects a circular area.
    *
    * @param start  The start of the segment.
    * @param end    The end of the segment.
    * @param circle : The circle to test against.
    * @param fudge  An additional safety zone to leave when looking for collisions. (Probably set it to ship radius 0.5)
    * @return true if the segment intersects, false otherwise
    */
  def segmentCircleIntersect(start: Position,
                             end: Position,
                             circle: Entity,
                             fudge: Double)
    : Boolean = { // Parameterize the segment as start + t * (end - start),
    def square(num: Double): Double = num * num

    // and substitute into the equation of a circle
    // Solve for t
    val circleRadius = circle.radius
    val startX = start.getXPos
    val startY = start.getYPos
    val endX = end.getXPos
    val endY = end.getYPos
    val centerX = circle.getXPos
    val centerY = circle.getYPos
    val dx = endX - startX
    val dy = endY - startY
    val a = square(dx) + square(dy)
    if (a == 0.0) { // Start and end are the same point
      start.getDistanceTo(circle) <= circleRadius + fudge
    } else {
      val b = -2 * (square(startX) - (startX * endX) - (startX * centerX) + (endX * centerX) + square(
        startY) - (startY * endY) - (startY * centerY) + (endY * centerY))

      // Time along segment when closest to the circle (vertex of the quadratic)
      val t = Math.min(-b / (2 * a), 1.0)
      if (t < 0) {
        false
      } else {
        val closestX = startX + dx * t
        val closestY = startY + dy * t
        val closestDistance =
          new Position(closestX, closestY).getDistanceTo(circle)
        closestDistance <= circleRadius + fudge
      }
    }
  }
}
