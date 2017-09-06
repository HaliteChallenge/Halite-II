package hlt;


public class Navigation {

    private Ship ship;
    private Position target;

    public Navigation(Ship ship, Position target) {
        this.ship = ship;
        this.target = target;
    }

    public ThrustMove navigateToDock(GameMap gameMap, double maxSpeed, Planet planet) {
        final int maxCorrections = Constants.MAX_CORRECTIONS;
        final boolean avoidObstacles = true;
        final double planetRadius = planet.getRadius();

        return navigateTowards(gameMap, target, maxSpeed, avoidObstacles, maxCorrections, planetRadius);
    }

    public ThrustMove navigateTowards(GameMap gameMap, Position target, double maxAccel, boolean avoidObstacles, int maxCorrections, double planetRadius) {
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
            final ThrustMove.ThrustVector newThrustVector = gameMap.adjustForCollision(shipPos, angle, (short) maxAccel, maxCorrections);
            final double angleRad = newThrustVector.getAngleRad();

            final double newTargetPosX = Math.cos(angleRad * distance);
            final double newTargetPosY = Math.sin(angleRad * distance);

            final Position newTarget = new Position(shipPos.getXPos() + newTargetPosX, shipPos.getYPos() + newTargetPosY);
            return navigateTowards(gameMap, newTarget, maxAccel, true, (maxCorrections - 1), planetRadius);
        }


        final double shipVelocity = ship.getVelocity().getMagnitude();
        short thrust = 0;

        if (shipVelocity != 0) {
            if (distance < (maxAccel + shipVelocity)) {
                thrust = (short) (distance - shipVelocity);
            }
            else {
                thrust = (short) (maxAccel - shipVelocity);
            }

            if (thrust > 5) {
                thrust = 5;
            }
        }

        return new ThrustMove(ship, angle, thrust);
    }
}
