package hlt;


public class Collision {
    /**
     * Test whether a given line segment intersects a circular area.
     *
     * @param start  The start of the segment.
     * @param end    The end of the segment.
     * @param circle: The circle to test against.
     * @param fudge  An additional safety zone to leave when looking for collisions. (Probably set it to ship radius 0.5)
     * @return true if the segment intersects, false otherwise
     */
    public static boolean segmentCircleIntersect(Position start, Position end, Entity circle, double fudge) {
        // Derived with SymPy
        // Parameterize the segment as start + t * (end - start),
        // and substitute into the equation of a circle
        // Solve for t
        final Position circleCenter = circle.getPosition();
        final double circleRadius = circle.getRadius();
        final double startX = start.getXPos();
        final double startY = start.getYPos();
        final double endX = end.getXPos();
        final double endY = end.getYPos();
        final double centerX = circleCenter.getXPos();
        final double centerY = circleCenter.getXPos();
        final double dx = startX - endX;
        final double dy = startY - endY;

        final double a = square(dx) + square(dy);

        final double b = -2 * (square(startX) - (startX * endX)
                            - (startX * centerX) + (endX * centerX)
                            + square(startY) - (startY * endY)
                            - (startY * centerY) + (endY * centerY));

        if (a == 0.0) {
            // Start and end are the same point
            return start.getDistanceTo(circleCenter) <= circleRadius + fudge;
        }

        // Time along segment when closest to the circle (vertex of the quadratic)
        final double t = Math.min(-b / (2 * a), 1.0);
        if (t < 0) {
            return false;
        }

        final double closestX = startX + dx * t;
        final double closestY = startY + dy * t;
        final double closestDistance = new Position(closestX, closestY).getDistanceTo(circleCenter);

        return closestDistance <= circleRadius + fudge;
    }

    public static double square(double num) {
        return num * num;
    }
}
