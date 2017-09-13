package hlt;

public class Entity extends Position {

    private final Short owner;
    private final long id;
    private final short health;
    private final double radius;

    public Entity(Short owner, long id, double xPos, double yPos, short health, double radius) {
        super(xPos, yPos);
        this.owner = owner;
        this.id = id;
        this.health = health;
        this.radius = radius;
    }

    public Short getOwner() {
        return owner;
    }

    public long getId() {
        return id;
    }

    public short getHealth() {
        return health;
    }

    public double getRadius() {
        return radius;
    }
}
