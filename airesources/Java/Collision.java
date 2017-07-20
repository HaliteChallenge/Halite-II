public class Collision {
    /**
     * Test whether a given line segment intersects a circular area.
     * @param start The start of the segment.
     * @param end The end of the segment.
     * @param center The center of the circular area.
     * @param radius The radius of the circular area.
     * @param fudge An additional safety zone to leave when looking for collisions.
     * @return true if the segment intersects, false otherwise
     */
    public static boolean segmentCircleTest(Position start, Position end, Position center, double radius, double fudge) {
        // Derived with SymPy
        // Parameterize the segment as start + t * (end - start),
        // and substitute into the equation of a circle
        // Solve for t
        double dx = end.getXPos() - start.getXPos();
        double dy = end.getYPos() - start.getYPos();

        double a = Math.pow(dx, 2) + Math.pow(dy, 2);
        double b = -2 * (Math.pow(start.getXPos(), 2) - start.getXPos()*end.getXPos() -
                start.getXPos()*center.getXPos() + end.getXPos()*center.getXPos() +
                Math.pow(start.getYPos(), 2) - start.getYPos()*end.getYPos() -
                start.getYPos()*center.getYPos() + end.getYPos()*center.getYPos());
        double c = Math.pow(start.getXPos() - center.getXPos(), 2) +
                Math.pow(start.getYPos() - center.getYPos(), 2);

        if (a == 0.0) {
            // Start and end are the same point
            return Movement.getDistance(start, center) <= radius + fudge;
        }

        // Time along segment when closest to the circle (vertex of the quadratic)
        double t = Math.min(-b / (2 * a), 1.0);
        if (t < 0) {
            DebugLog.debug(String.format("Collision test: %s %s vs %s %f, false", start, end, center, radius));
            return false;
        }

        double closest_x = start.getXPos() + dx * t;
        double closest_y = start.getYPos() + dy * t;
        Position closest_location = new Position(closest_x, closest_y);
        double closest_distance = Movement.getDistance(closest_location, center);

        DebugLog.debug(String.format("Collision test: %s %s vs %s %f, %b", start, end, center, radius, closest_distance <= radius + fudge));
        return closest_distance <= radius + fudge;
    }
}
