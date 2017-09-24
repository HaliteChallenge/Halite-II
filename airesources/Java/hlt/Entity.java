package hlt;

public class Entity extends Position {

    private final Short owner;
    private final long id;
    private final short health;
    private final double radius;

    public Entity(final Short owner, final long id, final double xPos, final double yPos, final short health, final double radius) {
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

    @Override
    public String toString() {
        return "Entity[" +
                super.toString() +
                ", owner=" + owner +
                ", id=" + id +
                ", health=" + health +
                ", radius=" + radius +
                "]";
    }
}
