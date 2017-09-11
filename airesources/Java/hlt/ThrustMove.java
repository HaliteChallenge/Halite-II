package hlt;


public class ThrustMove extends Move {

    private final int angleDeg;
    private final int thrust;

    public static class ThrustVector {
        private final int angleDeg;
        private final int thrust;

        public ThrustVector(int angleDeg, int thrust) {
            this.angleDeg = angleDeg;
            this.thrust = thrust;
        }
    }

    public ThrustMove(Ship ship, int angleDeg, int thrust) {
        super(MoveType.Thrust, ship);
        this.thrust = thrust;
        this.angleDeg = angleDeg;
    }

    public ThrustMove(Ship ship, ThrustVector direction) {
        this(ship, direction.angleDeg, direction.thrust);
    }

    public int getAngle() {
        return angleDeg;
    }

    public int getThrust() {
        return thrust;
    }
}
