package hlt;


public class ThrustMove extends Move {

    private final short angleDeg;
    private final short thrust;

    public static class ThrustVector {
        private final double angleRad;
        private final short thrust;

        public ThrustVector(double angleRad, short thrust) {
            this.angleRad = angleRad;
            this.thrust = thrust;
        }

        public double getAngleRad() {
            return angleRad;
        }
    }

    public ThrustMove(Ship ship, double angleRad, short thrust) {
        super(MoveType.Thrust, ship);
        this.thrust = thrust;

        final short angleDegUnclipped = (short)((angleRad * 180 / Math.PI) % 360);

        if (angleDegUnclipped < 0) {
            angleDeg = (short)(angleDegUnclipped + 360);
        } else {
            angleDeg = angleDegUnclipped;
        }
    }

    public ThrustMove(Ship ship, ThrustVector direction) {
        this(ship, direction.angleRad, direction.thrust);
    }

    public short getAngle() {
        return angleDeg;
    }

    public short getThrust() {
        return thrust;
    }
}
