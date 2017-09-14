package hlt;

public class ThrustMove extends Move {

    private final int angleDeg;
    private final int thrust;

    public ThrustMove(final Ship ship, final int angleDeg, final int thrust) {
        super(MoveType.Thrust, ship);
        this.thrust = thrust;
        this.angleDeg = angleDeg;
    }

    public int getAngle() {
        return angleDeg;
    }

    public int getThrust() {
        return thrust;
    }
}
