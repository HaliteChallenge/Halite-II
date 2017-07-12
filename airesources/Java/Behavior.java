public class Behavior {
    private static final short MAX_ADJUSTMENT_TRIES = 2;
    public enum BehaviorType {Brake, Warp};
    public enum State {Moving, Braking, Stopped}
    private BehaviorType type;
    private long shipId;
    private Position target;
    private State state;

    public Behavior(long shipId, BehaviorType type, Position target, State state) {
        this.shipId = shipId;
        this.type = type;
        this.target = target;
        this.state = state;
    }

    public boolean isFinished(GameMap gameMap) {
        if (!gameMap.getMyPlayer().getShips().containsKey(shipId))
            return true;

        Ship ship = gameMap.getMyPlayer().getShip(shipId);
        switch (type) {
            case Brake:
                return ship.getVelocity().getXVelocity() == 0 &&
                        ship.getVelocity().getYVelocity() == 0;
            case Warp:
                return ship.getPosition() == this.target;
            default:
                return true;
        }
    }


    public Move brake(GameMap gameMap, double speed, double angle, int maxAcceleration) {
        Ship ship = gameMap.getShip(gameMap.getMyPlayerId(), shipId);
        short thrust = (short) Math.min(maxAcceleration, speed);
        return new ThrustMove(ship, angle + Math.PI, thrust);
    }

    public Move next(GameMap gameMap) {
        if (!gameMap.getMyPlayer().getShips().containsKey(shipId))
            assert(false);

        Ship ship = gameMap.getShip(gameMap.getMyPlayerId(), shipId);
        int maxAccel = Constants.MAX_ACCELERATION;
        double speed = ship.getVelocity().getMagnitude();
        double angle = ship.getVelocity().getAngle();
        switch (type) {
            case Brake: {
                return brake(gameMap, speed, angle, maxAccel);
            }
            case Warp: {
                double distance = Movement.getDistance(ship.getPosition(), this.target);
                int turnsLeft = 10000;
                if (speed > 0)
                    turnsLeft = (int)(distance / speed);

                double turnsToDecelerate = speed / (Constants.MAX_ACCELERATION + Constants.DRAG);

                if (this.state == State.Stopped || (this.state == State.Braking && speed == 0)) {
                    this.state = State.Stopped;
                    double newAngle = Movement.orientTowards(ship.getPosition(), this.target);
                    return new ThrustMove(
                            gameMap.getShip(gameMap.getMyPlayerId(), shipId),
                            gameMap.adjustForCollision(ship.getPosition(), (short) 2,
                                    (short) newAngle, MAX_ADJUSTMENT_TRIES));
                }
                else if (turnsLeft <= turnsToDecelerate || this.state == State.Braking) {
                    this.state = State.Braking;
                    return brake(gameMap, speed, angle, maxAccel);
                }
                else {
                    double newAngle = Movement.orientTowards(ship.getPosition(), target);
                    short thrust = (short)(Math.max(1, Math.min(maxAccel, (distance / 30 * maxAccel))));
                    return new ThrustMove(gameMap.getShip(gameMap.getMyPlayerId(), shipId), newAngle, thrust);
                }
            }
            default:
                return null;
        }
    }

    void cancel() {
        type = BehaviorType.Brake;
    }
}
