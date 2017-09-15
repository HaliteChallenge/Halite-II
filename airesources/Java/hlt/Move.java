package hlt;

public class Move {

    public enum MoveType { Noop, Thrust, Dock, Undock }

    private final MoveType type;
    private final Ship ship;

    public Move(final MoveType type, final Ship ship) {
        this.type = type;
        this.ship = ship;
    }

    public MoveType getType() {
        return type;
    }

    public Ship getShip() {
        return ship;
    }
}
