package hlt;


public class Movement {

    public static double getDistance(Position start, Position target) {
        final double dx = start.getXPos() - target.getXPos();
        final double dy = start.getYPos() - target.getYPos();
        return Math.sqrt(Math.pow(dx, 2) + Math.pow(dy, 2));
    }

    public static double orientTowardsInDeg(Position start, Position target) {
        final double dx = target.getXPos() - start.getXPos();
        final double dy = target.getYPos() - start.getYPos();

        return Math.toDegrees(Math.atan2(dy, dx)) % 360;
    }

    public static double orientTowardsInDeg(Ship ship, Entity target) {
        return orientTowardsInDeg(ship.getPosition(), target.getPosition());
    }

    public static boolean canDock(Ship ship, Planet planet) {
        return getDistance(ship.getPosition(), planet.getPosition())
                    <= Constants.DOCK_RADIUS + planet.getRadius();
    }
}
