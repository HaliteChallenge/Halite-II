package hlt;

public class Ship extends Entity {

    public enum DockingStatus { Undocked, Docking, Docked, Undocking }

    private final Velocity velocity;
    private final DockingStatus dockingStatus;
    private final long dockedPlanet;
    private final short dockingProgress;
    private final short weaponCooldown;

    public Ship(final short owner, final long id, final double xPos, final double yPos,
                final short health, final Velocity velocity, final DockingStatus dockingStatus,
                final long dockedPlanet, final short dockingProgress, final short weaponCooldown) {

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

    public boolean canDock(final Planet planet) {
        return getDistanceTo(planet) <= Constants.DOCK_RADIUS + planet.getRadius();
    }
}
