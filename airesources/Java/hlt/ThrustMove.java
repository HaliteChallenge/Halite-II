public class ThrustMove extends Move {
    private short angle;
    private short thrust;

    public static class Pair {
        private final double angle;
        private final short thrust;

        public Pair(double angle, short thrust) {
            this.angle = angle;
            this.thrust = thrust;
        }
    }

    public ThrustMove(Ship ship, double angle, short thrust) {
        this.type = MoveType.Thrust;
        this.ship = ship;
        this.thrust = thrust;
        this.angle = (short)((angle * 180 / Math.PI) % 360);
        if (this.angle < 0) this.angle += 360;
    }

    public ThrustMove(Ship ship, Pair direction) {
        this(ship, direction.angle, direction.thrust);
    }

    public short getAngle() {
        return angle;
    }

    public short getThrust() {
        return thrust;
    }

}
