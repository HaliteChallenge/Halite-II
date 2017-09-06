package hlt;

import java.util.ArrayList;
import java.util.LinkedList;


public class Planet extends Entity {

    private Short owner;
    private double radius;
    private short remainingProduction;
    private short currentProduction;
    private short dockingSpots;
    private ArrayList<Long> dockedShips;

    public Planet(LinkedList<String> planetMetadata) {
        super((short) 0, planetMetadata, Type.Planet);
        radius = Double.parseDouble(planetMetadata.pop());
        dockingSpots = Short.parseShort(planetMetadata.pop());
        currentProduction = Short.parseShort(planetMetadata.pop());
        remainingProduction = Short.parseShort(planetMetadata.pop());

        if(Integer.parseInt(planetMetadata.pop()) == 1) {
            owner = Short.parseShort(planetMetadata.pop());
        } else {
            owner = null;
            planetMetadata.pop();
        }

        final int dockedShipCount = Integer.parseInt(planetMetadata.pop());
        dockedShips = new ArrayList<>(dockedShipCount);
        for(int i = 0; i < dockedShipCount; i++) {
            dockedShips.add(Long.parseLong(planetMetadata.pop()));
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

    public ArrayList<Long> getDockedShips() {
        return dockedShips;
    }

    @Override
    public double getRadius() {
        return radius;
    }

    public boolean isFull() {
        return dockedShips.size() == dockingSpots;
    }
}
