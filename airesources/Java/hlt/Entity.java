package hlt;

import java.util.LinkedList;


public abstract class Entity {

    final private EntityId entityId;
    private Position position;
    private short health;
    public enum Type { Planet, Ship }

    public Entity(short owner, LinkedList<String> metadata, Type type) {
        this.entityId = new EntityId(owner, Long.parseLong(metadata.pop()), type);
        this.position = new Position(Double.parseDouble(metadata.pop()), Double.parseDouble(metadata.pop()));
        this.health = Short.parseShort(metadata.pop());
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

    public abstract double getRadius();

    public EntityId getEntityId() {
        return entityId;
    }
}
