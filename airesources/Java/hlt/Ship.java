package hlt;

import java.util.LinkedList;


public class Ship extends Entity {

    public enum DockingStatus { Undocked, Docking, Docked, Undocking }
    private Velocity velocity;
    private short weaponCooldown;
    private DockingStatus dockingStatus;
    private short dockingProgress;
    private long dockedPlanet;

    public Ship(short owner, LinkedList<String> shipMetadata) {
        super(owner, shipMetadata, Type.Ship);
        velocity = new Velocity(Double.parseDouble(shipMetadata.pop()), Double.parseDouble(shipMetadata.pop()));
        dockingStatus = DockingStatus.values()[Short.parseShort(shipMetadata.pop())];
        dockedPlanet = Long.parseLong(shipMetadata.pop());
        dockingProgress = Short.parseShort(shipMetadata.pop());
        weaponCooldown = Short.parseShort(shipMetadata.pop());
    }

    public Velocity getVelocity() {
        return velocity;
    }

    public short getWeaponCooldown() {
        return weaponCooldown;
    }

    public DockingStatus getDockingStatus() {
        return dockingStatus;
    }

    public short getDockingProgress() {
        return dockingProgress;
    }

    public long getDockedPlanet() {
        return dockedPlanet;
    }

    static LinkedList<Ship> getShipList(short owner, LinkedList<String> shipsMetadata) {
        final long numberOfShips = Long.parseLong(shipsMetadata.pop());
        LinkedList<Ship> ships = new LinkedList<>();

        for(int i = 0; i < numberOfShips; i++) {
            ships.add(new Ship(owner, shipsMetadata));
        }
        return ships;
    }

    public boolean canDock(Planet planet) {
        return getPosition().getDistanceTo(planet.getPosition()) <= Constants.DOCK_RADIUS + planet.getRadius();
    }

    @Override
    public double getRadius() {
        return Constants.SHIP_RADIUS;
    }
}
