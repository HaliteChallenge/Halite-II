package hlt;

import java.util.List;

public class Planet extends Entity {

    private final Short owner;
    private final short remainingProduction;
    private final short currentProduction;
    private final short dockingSpots;
    private final List<Long> dockedShips;

    public Planet(Short owner, long id, Position position, short health, double radius,
                  short dockingSpots, short currentProduction, short remainingProduction, List<Long> dockedShips) {

        super(owner, id, position, health, radius);

        this.dockingSpots = dockingSpots;
        this.currentProduction = currentProduction;
        this.remainingProduction = remainingProduction;
        this.owner = owner;
        this.dockedShips = dockedShips;
    }

    public short getRemainingProduction() {
        return remainingProduction;
    }

    public short getCurrentProduction() {
        return currentProduction;
    }

    public short getDockingSpots() {
        return dockingSpots;
    }

    public List<Long> getDockedShips() {
        return dockedShips;
    }

    public boolean isFull() {
        return dockedShips.size() == dockingSpots;
    }

    public boolean isOwned() {
        return owner != null;
    }
}
