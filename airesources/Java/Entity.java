public abstract class Entity {
    protected Position position;
    protected short health;
    protected short radius;
    protected EntityId id;
    public enum Type {Planet, Ship};

    public boolean is_alive() {
        return health > 0;
    }

    public Position getPosition() {
        return position;
    }

    public void setPosition(Position position) {
        this.position = position;
    }

    public short getHealth() {
        return health;
    }

    public void setHealth(short health) {
        this.health = health;
    }

    public short getRadius() {
        return radius;
    }

    public EntityId getId() {
        return id;
    }
}
