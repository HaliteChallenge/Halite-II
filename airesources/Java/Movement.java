public class Movement {

    public static double getDistance(Position start, Position target) {
        double dx = start.getXPos() - target.getXPos();
        double dy = start.getYPos() - target.getYPos();
        return Math.sqrt(Math.pow(dx, 2) + Math.pow(dy, 2));
    }

    public static double orientTowards(Position start, Position target) {
        double dx = target.getXPos() - start.getXPos();
        double dy = target.getYPos() - start.getYPos();

        double angleRad = Math.atan2(dy, dx);
        if (angleRad < 0)
            angleRad += 2 * Math.PI;

        return angleRad;
    }

    public static double orientTowards(Ship ship, Entity target) {
        return orientTowards(ship.getPosition(), target.getPosition());
    }
    public static boolean canDock(Ship ship, Planet planet) {
        return getDistance(ship.getPosition(), planet.getPosition()) <= Constants.MAX_DOCKING_DISTANCE + planet.getRadius();
    }

}
