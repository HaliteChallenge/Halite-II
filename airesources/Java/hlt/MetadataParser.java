package hlt;

import java.util.ArrayList;
import java.util.LinkedList;

public class MetadataParser {

    public static LinkedList<Ship> getShipList(short owner, LinkedList<String> shipsMetadata) {
        final long numberOfShips = Long.parseLong(shipsMetadata.pop());
        LinkedList<Ship> ships = new LinkedList<>();

        for(int i = 0; i < numberOfShips; i++) {
            ships.add(newShipFromMetadata(owner, shipsMetadata));
        }
        return ships;
    }

    private static Ship newShipFromMetadata(final short owner, final LinkedList<String> metadata) {
        final long id = Long.parseLong(metadata.pop());
        final Position position = new Position(Double.parseDouble(metadata.pop()), Double.parseDouble(metadata.pop()));
        final short health = Short.parseShort(metadata.pop());

        final Velocity velocity = new Velocity(Double.parseDouble(metadata.pop()), Double.parseDouble(metadata.pop()));
        final Ship.DockingStatus dockingStatus = Ship.DockingStatus.values()[Short.parseShort(metadata.pop())];
        final long dockedPlanet = Long.parseLong(metadata.pop());
        final short dockingProgress = Short.parseShort(metadata.pop());
        final short weaponCooldown = Short.parseShort(metadata.pop());

        return new Ship(owner, id, position, health, velocity, dockingStatus, dockedPlanet, dockingProgress, weaponCooldown);
    }

    public static Planet newPlanetFromMetadata(final LinkedList<String> metadata) {
        final long id = Long.parseLong(metadata.pop());
        final Position position = new Position(Double.parseDouble(metadata.pop()), Double.parseDouble(metadata.pop()));
        final short health = Short.parseShort(metadata.pop());

        final double radius = Double.parseDouble(metadata.pop());
        final short dockingSpots = Short.parseShort(metadata.pop());
        final short currentProduction = Short.parseShort(metadata.pop());
        final short remainingProduction = Short.parseShort(metadata.pop());

        final Short owner;
        if (Integer.parseInt(metadata.pop()) == 1) {
            owner = Short.parseShort(metadata.pop());
        }
        else {
            owner = null;
            metadata.pop();
        }

        final int dockedShipCount = Integer.parseInt(metadata.pop());
        final ArrayList<Long> dockedShips = new ArrayList<>(dockedShipCount);
        for(int i = 0; i < dockedShipCount; i++) {
            dockedShips.add(Long.parseLong(metadata.pop()));
        }

        return new Planet(owner, id, position, health, radius, dockingSpots, currentProduction, remainingProduction, dockedShips);
    }

    public static Short parsePlayerNum(final LinkedList<String> metadata) {
        return Short.parseShort(metadata.pop());
    }

    public static Short parsePlayerId(final LinkedList<String> metadata) {
        return Short.parseShort(metadata.pop());
    }
}
