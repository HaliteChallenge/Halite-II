package halitejavabot;

public class Move {
    public enum MoveType {Noop, Thrust, Dock, Undock}
    protected MoveType type;
    protected Ship ship;

    public Move() {
        this.type = MoveType.Noop;
        this.ship = null;
    }

    public MoveType getType() {
        return type;
    }

    public Ship getShip() {
        return ship;
    }

}
