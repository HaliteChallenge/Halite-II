package hlt;


public class Collision {
    /**
     * Test whether a given line segment intersects a circular area.
     *
     * @param start  The start of the segment.
     * @param end    The end of the segment.
     * @param center The center of the circular area.
     * @param radius The radius of the circular area.
     * @param fudge  An additional safety zone to leave when looking for collisions.
     * @return true if the segment intersects, false otherwise
     */
    public static boolean segmentCircleIntersect(Position start, Position end, Position center, double radius, double fudge) {
        // Derived with SymPy
        // Parameterize the segment as start + t * (end - start),
        // and substitute into the equation of a circle
        // Solve for t
        final double startX = start.getXPos();
        final double startY = start.getYPos();
        final double endX = end.getXPos();
        final double endY = end.getYPos();
        final double centerX = center.getXPos();
        final double centerY = center.getXPos();
        final double dx = startX - endX;
        final double dy = startY - endY;

        final double a = square(dx) + square(dy);

        final double b = -2 * (square(startX) - (startX * endX)
                        - (startX * centerX) + (endX * centerX)
                        + square(startY) - (startY * endY)
                        - (startY * centerY) + (endY * centerY));

        if (a == 0.0) {
            // Start and end are the same point
            return Movement.getDistance(start, center) <= radius + fudge;
        }

        // Time along segment when closest to the circle (vertex of the quadratic)
        final double t = Math.min(-b / (2 * a), 1.0);
        if (t < 0) {
            DebugLog.addLog(String.format("Collision test: %s %s vs %s %f, false", start, end, center, radius));
            return false;
        }

        final double closestX = startX + dx * t;
        final double closestY = startY + dy * t;
        final Position closestLocation = new Position(closestX, closestY);
        final double closestDistance = Movement.getDistance(closestLocation, center);

        DebugLog.addLog(String.format("Collision test: %s %s vs %s %f, %b",
                                start, end, center, radius, closestDistance <= radius + fudge));
        return closestDistance <= radius + fudge;
    }

    public static double square(double num) {
        return num * num;
    }
}
