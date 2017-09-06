import java.util.LinkedList;

public class Ship extends Entity {
    public enum DockingStatus {Undocked, Docking, Docked, Undocking};
    public class Velocity {
        private double xVelocity;
        private double yVelocity;

        public Velocity(double xVelocity, double yVelocity) {
            this.xVelocity = xVelocity;
            this.yVelocity = yVelocity;
        }

        public double getXVelocity() {
            return xVelocity;
        }

        public double getYVelocity() {
            return yVelocity;
        }

        double getMagnitude() {
            return Math.sqrt(Math.pow(xVelocity, 2) + Math.pow(yVelocity, 2));
        }
        double getAngle() {
            return Math.atan2(yVelocity, xVelocity);
        }
    }
    private Velocity velocity;
    private short weaponCooldown;
    private DockingStatus dockingStatus;
    private short dockingProgress;
    private long dockedPlanet;

    public Ship(short owner, LinkedList<String> shipMetadata) {
        this.id = new EntityId(owner, Long.parseLong(shipMetadata.pop()), Entity.Type.Ship);
        this.position = new Position(Double.parseDouble(shipMetadata.pop()), Double.parseDouble(shipMetadata.pop()));
        this.health = Short.parseShort(shipMetadata.pop());
        this.velocity = new Velocity(Double.parseDouble(shipMetadata.pop()), Double.parseDouble(shipMetadata.pop()));
        this.dockingStatus = DockingStatus.values()[Short.parseShort(shipMetadata.pop())];
        this.dockedPlanet = Long.parseLong(shipMetadata.pop());
        this.dockingProgress = Short.parseShort(shipMetadata.pop());
        this.weaponCooldown = Short.parseShort(shipMetadata.pop());
        this.radius = Constants.SHIP_RADIUS;
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

    static LinkedList<Ship> parseShips(short owner, LinkedList<String> shipsMetadata) {
        long numberOfShips = Long.parseLong(shipsMetadata.pop());
        LinkedList<Ship> ships = new LinkedList<>();
        for(int i = 0; i < numberOfShips; i++) {
            ships.add(new Ship(owner, shipsMetadata));
        }
        return ships;
    }

}
