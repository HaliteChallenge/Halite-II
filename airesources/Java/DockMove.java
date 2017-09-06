package halitejavabot;

public class DockMove extends Move {
    private EntityId destination;

    public DockMove(Ship ship, Planet planet) {
        this.type = Move.MoveType.Dock;
        this.ship = ship;
        this.destination = planet.getId();
    }

    public EntityId getDestination() {
        return destination;
    }
}
