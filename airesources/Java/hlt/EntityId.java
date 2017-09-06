package hlt;


public class EntityId {

    private short owner;
    private long id;
    private Entity.Type type;

    public EntityId(short owner, long id, Entity.Type type) {
        this.owner = owner;
        this.id = id;
        this.type = type;
    }

    public short getOwner() {
        return owner;
    }

    public void setOwner(short owner) {
        this.owner = owner;
    }

    public long getId() {
        return id;
    }

    public void setId(long id) {
        this.id = id;
    }

    public Entity.Type getType() {
        return type;
    }

    public void setType(Entity.Type type) {
        this.type = type;
    }
}
