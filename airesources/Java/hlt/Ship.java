package hlt;

public class Ship extends Entity {

    public enum DockingStatus { Undocked, Docking, Docked, Undocking }

    private final Velocity velocity;
    private final DockingStatus dockingStatus;
    private final long dockedPlanet;
    private final short dockingProgress;
    private final short weaponCooldown;

    public Ship(short owner, long id, double xPos, double yPos, short health, Velocity velocity,
                DockingStatus dockingStatus, long dockedPlanet, short dockingProgress, short weaponCooldown) {

        super(owner, id, xPos, yPos, health, Constants.SHIP_RADIUS);

        this.velocity = velocity;
        this.dockingStatus = dockingStatus;
        this.dockedPlanet = dockedPlanet;
        this.dockingProgress = dockingProgress;
        this.weaponCooldown = weaponCooldown;
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

    public boolean canDock(Planet planet) {
        return getDistanceTo(planet) <= Constants.DOCK_RADIUS + planet.getRadius();
    }
}
