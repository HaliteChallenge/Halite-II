package hlt;

public class Navigation {

    private Ship ship;
    private Entity target;

    public Navigation(final Ship ship, final Entity target) {
        this.ship = ship;
        this.target = target;
    }

    public ThrustMove navigateToDock(final GameMap gameMap, final double maxThrust) {
        final int maxCorrections = Constants.MAX_CORRECTIONS;
        final boolean avoidObstacles = true;
        final int angularStep = 1;
        final Position targetPos = ship.getClosestPoint(target);

        return navigateTowards(gameMap, targetPos, maxThrust, avoidObstacles, maxCorrections, angularStep);
    }

    public ThrustMove navigateTowards(final GameMap gameMap, final Position targetPos, final double maxThrust,
                                      final boolean avoidObstacles, final int maxCorrections, final int angularStep) {
        if (maxCorrections <= 0) {
            return null;
        }

        final double distance = ship.getDistanceTo(targetPos);
        final double angleDeg = ship.orientTowardsInDeg(targetPos);

        if (avoidObstacles && !gameMap.objectsBetween(ship, targetPos).isEmpty()) {
            final double newTargetDx = Math.cos(Math.toRadians(angleDeg + angularStep)) * distance;
            final double newTargetDy = Math.sin(Math.toRadians(angleDeg + angularStep)) * distance;
            final Position newTarget = new Position(ship.getXPos() + newTargetDx, ship.getYPos() + newTargetDy);

            return navigateTowards(gameMap, newTarget, maxThrust, true, (maxCorrections - 1), angularStep);
        }

        final double thrust;
        if (distance < maxThrust) {
            thrust = distance;
        }
        else {
            thrust = maxThrust;
        }

        return new ThrustMove(ship, (int)(angleDeg + 0.5), (int) (thrust + 0.5));
    }
}
