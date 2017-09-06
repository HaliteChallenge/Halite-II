import java.util.LinkedList;
import java.util.Vector;

public class Planet extends Entity {
    private Short owner;
    private short remainingProduction;
    private short currentProduction;
    private short dockingSpots;
    private Vector<Long> dockedShips;

    public Planet(LinkedList<String> planetMetadata) {
        this.id = new EntityId((short)0, Long.parseLong(planetMetadata.pop()), Entity.Type.Planet);
        this.position = new Position(Double.parseDouble(planetMetadata.pop()), Double.parseDouble(planetMetadata.pop()));
        this.health = Short.parseShort(planetMetadata.pop());
        this.radius = Double.parseDouble(planetMetadata.pop());
        this.dockingSpots = Short.parseShort(planetMetadata.pop());
        this.currentProduction = Short.parseShort(planetMetadata.pop());
        this.remainingProduction = Short.parseShort(planetMetadata.pop());
        if(Integer.parseInt(planetMetadata.pop()) == 1) {
            this.owner = Short.parseShort(planetMetadata.pop());
        } else {
            this.owner = null;
            planetMetadata.pop();
        }
        int dockedShips = Integer.parseInt(planetMetadata.pop());
        this.dockedShips = new Vector<>(dockedShips);
        for(int i = 0; i < this.dockedShips.capacity(); i++) {
            this.dockedShips.add(Long.parseLong(planetMetadata.pop()));
        }
    }

    public Short getOwner() {
        return owner;
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

    public Vector<Long> getDockedShips() {
        return dockedShips;
    }

}
