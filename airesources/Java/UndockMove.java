package halitejavabot;

public class UndockMove extends Move {
    public UndockMove(Ship ship) {
        this.type = MoveType.Undock;
        this.ship = ship;
    }

}
