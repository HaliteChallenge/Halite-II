package hlt;

import java.util.List;

public class Planet extends Entity {

    private final short remainingProduction;
    private final short currentProduction;
    private final short dockingSpots;
    private final List<Long> dockedShips;

    public Planet(final Short owner, final long id, final double xPos, final double yPos,
                  final short health, final double radius, final short dockingSpots,
                  final short currentProduction, final short remainingProduction,
                  final List<Long> dockedShips) {

        super(owner, id, xPos, yPos, health, radius);

        this.dockingSpots = dockingSpots;
        this.currentProduction = currentProduction;
        this.remainingProduction = remainingProduction;
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
        return getOwner() != null;
    }

    @Override
    public String toString() {
        return "Planet[" +
                super.toString() +
                ", remainingProduction=" + remainingProduction +
                ", currentProduction=" + currentProduction +
                ", dockingSpots=" + dockingSpots +
                ", dockedShips=" + dockedShips +
                "]";
    }
}
