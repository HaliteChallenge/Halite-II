package halite

object Collision {
    /**
     * Test whether a given line segment intersects a circular area.
     *
     * @param start  The start of the segment.
     * @param end    The end of the segment.
     * @param circle The circle to test against.
     * @param fudge  An additional safety zone to leave when looking for collisions. Probably set it to ship radius 0.5.
     * @return true if the segment intersects, false otherwise
     */
    fun segmentCircleIntersect(start: Position, end: Position, circle: Entity, fudge: Double): Boolean {
        // Parameterize the segment as start + t * (end - start),
        // and substitute into the equation of a circle
        // Solve for t
        val circleRadius = circle.radius
        val startX = start.xPos
        val startY = start.yPos
        val endX = end.xPos
        val endY = end.yPos
        val centerX = circle.xPos
        val centerY = circle.yPos
        val dx = endX - startX
        val dy = endY - startY

        val a = square(dx) + square(dy)

        val b = -2 * ((((square(startX) - startX * endX
                - startX * centerX) + endX * centerX
                + square(startY)) - startY * endY
                - startY * centerY) + endY * centerY)

        if (a == 0.0) {
            // Start and end are the same point
            return start.getDistanceTo(circle) <= circleRadius + fudge
        }

        // Time along segment when closest to the circle (vertex of the quadratic)
        val t = Math.min(-b / (2 * a), 1.0)
        if (t < 0) {
            return false
        }

        val closestX = startX + dx * t
        val closestY = startY + dy * t
        val closestDistance = Position(closestX, closestY).getDistanceTo(circle)

        return closestDistance <= circleRadius + fudge
    }

    private fun square(num: Double): Double {
        return num * num
    }
}
