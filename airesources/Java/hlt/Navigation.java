package hlt;

public class Navigation {

    private Ship ship;
    private Entity target;

    public Navigation(final Ship ship, final Entity target) {
        this.ship = ship;
        this.target = target;
    }

    public ThrustMove navigateToDock(final GameMap gameMap, final int maxThrust) {
        final int maxCorrections = Constants.MAX_CORRECTIONS;
        final boolean avoidObstacles = true;
        final double angularStepRad = Math.PI/180;
        final Position targetPos = ship.getClosestPoint(target);

        return navigateTowards(gameMap, targetPos, maxThrust, avoidObstacles, maxCorrections, angularStepRad);
    }

    public ThrustMove navigateTowards(final GameMap gameMap, final Position targetPos, final int maxThrust,
                                      final boolean avoidObstacles, final int maxCorrections, final double angularStepRad) {
        if (maxCorrections <= 0) {
            return null;
        }

        final double distance = ship.getDistanceTo(targetPos);
        final double angleRad = ship.orientTowardsInRad(targetPos);

        if (avoidObstacles && !gameMap.objectsBetween(ship, targetPos).isEmpty()) {
            final double newTargetDx = Math.cos(angleRad + angularStepRad) * distance;
            final double newTargetDy = Math.sin(angleRad + angularStepRad) * distance;
            final Position newTarget = new Position(ship.getXPos() + newTargetDx, ship.getYPos() + newTargetDy);

            return navigateTowards(gameMap, newTarget, maxThrust, true, (maxCorrections-1), angularStepRad);
        }

        final int thrust;
        if (distance < maxThrust) {
            // Do not round up, since overshooting might cause collision.
            thrust = (int) distance;
        }
        else {
            thrust = maxThrust;
        }

        final int angleDeg = Util.angleRadToDegClipped(angleRad);

        return new ThrustMove(ship, angleDeg, thrust);
    }
}
