package hlt;


public class NavigationV2 {

    private Ship ship;
    private Position target;

    public NavigationV2(Ship ship, Position target) {
        this.ship = ship;
        this.target = target;
    }

    public ThrustMove navigateToDock(GameMap gameMap, double maxSpeed, Planet planet) {
        final int maxCorrections = Constants.MAX_CORRECTIONS;
        final int angularStep = 1;
        final boolean avoidObstacles = true;
        final double planetRadius = planet.getRadius();

        return navigateTowards(gameMap, target, maxSpeed, avoidObstacles, maxCorrections, angularStep, planetRadius);
    }

    public ThrustMove navigateTowards(GameMap gameMap, Position target, double maxSpeed, boolean avoidObstacles, int maxCorrections, int angularStep, double planetRadius) {
        if (maxCorrections <= 0) {
            return null;
        }

        final Position shipPos = ship.getPosition();
        double distance = Movement.getDistance(shipPos, target);
        final double angle = Movement.orientTowardsInRad(shipPos, target);

        if (planetRadius != 0) {
            distance -= (ship.getRadius() + planetRadius + Constants.DOCK_RADIUS);
        }

        if (avoidObstacles && !gameMap.isPathable(shipPos, target)) {

            final double newTargetPosX = Math.cos(Math.toRadians(angle + angularStep)) * distance;
            final double newTargetPosY = Math.sin(Math.toRadians(angle + angularStep)) * distance;
            final Position newTarget = new Position(shipPos.getXPos() + newTargetPosX, shipPos.getYPos() + newTargetPosY);
            return navigateTowards(gameMap, newTarget, maxSpeed, true, (maxCorrections - 1), angularStep, planetRadius);
        }

        if (distance < maxSpeed) {
            maxSpeed = distance;
        }
        return new ThrustMove(ship, angle, (short) maxSpeed);
    }
}
