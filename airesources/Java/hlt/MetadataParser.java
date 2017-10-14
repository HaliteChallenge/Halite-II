package hlt;

import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;

public class MetadataParser {

    public static void populateShipList(final List<Ship> shipsOutput, final int owner, final Metadata shipsMetadata) {
        final long numberOfShips = Long.parseLong(shipsMetadata.pop());

        for(int i = 0; i < numberOfShips; ++i) {
            shipsOutput.add(newShipFromMetadata(owner, shipsMetadata));
        }
    }

    private static Ship newShipFromMetadata(final int owner, final Metadata metadata) {
        final int id = Integer.parseInt(metadata.pop());
        final double xPos = Double.parseDouble(metadata.pop());
        final double yPos = Double.parseDouble(metadata.pop());
        final int health = Integer.parseInt(metadata.pop());

        // Ignoring velocity(x,y) which is always (0,0) in current version.
        metadata.pop();
        metadata.pop();

        final Ship.DockingStatus dockingStatus = Ship.DockingStatus.values()[Integer.parseInt(metadata.pop())];
        final int dockedPlanet = Integer.parseInt(metadata.pop());
        final int dockingProgress = Integer.parseInt(metadata.pop());
        final int weaponCooldown = Integer.parseInt(metadata.pop());

        return new Ship(owner, id, xPos, yPos, health, dockingStatus, dockedPlanet, dockingProgress, weaponCooldown);
    }

    public static Planet newPlanetFromMetadata(final List<Integer> dockedShips, final Metadata metadata) {
        final int id = Integer.parseInt(metadata.pop());
        final double xPos = Double.parseDouble(metadata.pop());
        final double yPos = Double.parseDouble(metadata.pop());
        final int health = Integer.parseInt(metadata.pop());

        final double radius = Double.parseDouble(metadata.pop());
        final int dockingSpots = Integer.parseInt(metadata.pop());
        final int currentProduction = Integer.parseInt(metadata.pop());
        final int remainingProduction = Integer.parseInt(metadata.pop());

        final int hasOwner = Integer.parseInt(metadata.pop());
        final int ownerCandidate = Integer.parseInt(metadata.pop());
        final int owner;
        if (hasOwner == 1) {
            owner = ownerCandidate;
        } else {
            owner = -1; // ignore ownerCandidate
        }

        final int dockedShipCount = Integer.parseInt(metadata.pop());
        for (int i = 0; i < dockedShipCount; ++i) {
            dockedShips.add(Integer.parseInt(metadata.pop()));
        }

        return new Planet(owner, id, xPos, yPos, health, radius, dockingSpots,
                          currentProduction, remainingProduction, dockedShips);
    }

    public static int parsePlayerNum(final Metadata metadata) {
        return Integer.parseInt(metadata.pop());
    }

    public static int parsePlayerId(final Metadata metadata) {
        return Integer.parseInt(metadata.pop());
    }
}
