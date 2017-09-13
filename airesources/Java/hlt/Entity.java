package hlt;

public class Entity {

    private final Short owner;
    private final long id;
    private final Position position;
    private final short health;
    private final double radius;

    public Entity(Short owner, long id, Position position, short health, double radius) {
        this.owner = owner;
        this.id = id;
        this.position = position;
        this.health = health;
        this.radius = radius;
    }

    public Short getOwner() {
        return owner;
    }

    public long getId() {
        return id;
    }

    public Position getPosition() {
        return position;
    }

    public short getHealth() {
        return health;
    }

    public double getRadius() {
        return radius;
    }
}
