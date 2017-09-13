package hlt;

public class Navigation {

    private Ship ship;
    private Entity target;

    public Navigation(Ship ship, Entity target) {
        this.ship = ship;
        this.target = target;
    }

    public ThrustMove navigateToDock(GameMap gameMap, int thrust) {
        final int maxCorrections = Constants.MAX_CORRECTIONS;
        final boolean avoidObstacles = true;
        final int angularStep = 1;
        final Position targetPos = ship.getClosestPoint(target);

        return navigateTowards(gameMap, targetPos, thrust, avoidObstacles, maxCorrections, angularStep);
    }

    public ThrustMove navigateTowards(GameMap gameMap, Position targetPos, int thrust,
                                      boolean avoidObstacles, int maxCorrections, int angularStep) {
        if (maxCorrections <= 0) {
            return null;
        }

        final double distance = ship.getDistanceTo(targetPos);
        final double angleDeg = ship.orientTowardsInDeg(targetPos);

        if (avoidObstacles && !gameMap.objectsBetween(ship, targetPos).isEmpty()) {
            final double newTargetDx = Math.cos(Math.toRadians(angleDeg + angularStep)) * distance;
            final double newTargetDy = Math.sin(Math.toRadians(angleDeg + angularStep)) * distance;
            final Position newTarget = new Position(ship.getXPos() + newTargetDx, ship.getYPos() + newTargetDy);

            return navigateTowards(gameMap, newTarget, thrust, true, (maxCorrections - 1), angularStep);
        }

        if (distance < thrust) {
            thrust = (int)(distance + 0.5);
        }

        return new ThrustMove(ship, (int)(angleDeg + 0.5), thrust);
    }
}
