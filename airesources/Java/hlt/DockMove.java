package hlt;


public class DockMove extends Move {

    private EntityId destination;

    public DockMove(Ship ship, Planet planet) {
        super(MoveType.Dock, ship);
        destination = planet.getEntityId();
    }

    public EntityId getDestination() {
        return destination;
    }
}
