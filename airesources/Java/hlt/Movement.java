package hlt;


public class Movement {

    public static double getDistance(Position start, Position target) {
        final double dx = start.getXPos() - target.getXPos();
        final double dy = start.getYPos() - target.getYPos();
        return Math.sqrt(Math.pow(dx, 2) + Math.pow(dy, 2));
    }

    public static double orientTowardsInRad(Position start, Position target) {
        final double dx = target.getXPos() - start.getXPos();
        final double dy = target.getYPos() - start.getYPos();
        double angleRad = Math.atan2(dy, dx);

        if (angleRad < 0)
            angleRad += 2 * Math.PI;

        return angleRad;
    }

    public static double orientTowardsInRad(Ship ship, Entity target) {
        return orientTowardsInRad(ship.getPosition(), target.getPosition());
    }

    public static boolean canDock(Ship ship, Planet planet) {
        return getDistance(ship.getPosition(), planet.getPosition())
                    <= Constants.DOCK_RADIUS + planet.getRadius();
    }
}
