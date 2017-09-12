package hlt;


public class Navigation {

    private Ship ship;
    private Entity target;

    public Navigation(Ship ship, Entity target) {
        this.ship = ship;
        this.target = target;
    }

    public ThrustMove navigateToDock(GameMap gameMap, int maxThrust) {
        final int maxCorrections = Constants.MAX_CORRECTIONS;
        final boolean avoidObstacles = true;
        final int angularStep = 1;
        final Position targetPos = gameMap.getClosestPoint(ship, target);

        return navigateTowards(gameMap, targetPos, maxThrust, avoidObstacles, maxCorrections, angularStep);
    }

    public ThrustMove navigateTowards(GameMap gameMap, Position targetPos, int maxThrust, boolean avoidObstacles, int maxCorrections, int angularStep) {
        if (maxCorrections <= 0) {
            return null;
        }

        final Position shipPos = ship.getPosition();
        double distance = shipPos.getDistanceTo(targetPos);
        final double angleDeg = shipPos.orientTowardsInDeg(targetPos);
        int thrust = maxThrust;

        if (avoidObstacles && !gameMap.objectsBetween(shipPos, targetPos).isEmpty()) {
//      if (avoidObstacles && !gameMap.isPathable(shipPos, targetPos)) {
            final double newTargetDx = Math.cos(Math.toRadians(angleDeg + angularStep)) * distance;
            final double newTargetDy = Math.sin(Math.toRadians(angleDeg + angularStep)) * distance;
            final Position newTarget = new Position(shipPos.getXPos() + newTargetDx, shipPos.getYPos() + newTargetDy);

            return navigateTowards(gameMap, newTarget, maxThrust, true, (maxCorrections - 1), angularStep);
        }

        if (distance < maxThrust) {
            thrust = (int) (distance + 0.5);
        }

        if (thrust > Constants.DOCK_RADIUS) {
            thrust -= Constants.DOCK_RADIUS;
        }

        return new ThrustMove(ship, (int) (angleDeg + 0.5), thrust);
    }
}
