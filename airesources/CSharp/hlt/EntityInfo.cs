using System.Text;

/// <summary>
/// Entity Information
/// </summary>
public class EntityInfo {

    private int owner;
    private int id;
    private EntityType type;

    /// <summary>
    /// Constructor for EntityInfo
    /// </summary>
    /// <param name="owner">Owner of Entity</param>
    /// <param name="id">Id of Entity</param>
    /// <param name="type">Type of Entity(Planet, Ship)</param>
    public EntityInfo (int owner, int id, EntityType type) {
        this.owner = owner;
        this.id = id;
        this.type = type;
    }

    /// <summary>
    /// Owner of the Entity (Ship or Planet)
    /// </summary>
    /// <returns></returns>
    public int Owner {
        get {
            return owner;
        }
    }

    /// <summary>
    /// Id of the Entity (Ship or Planet)
    /// </summary>
    /// <returns></returns>
    public int Id {
        get {
            return id;
        }
    }

    /// <summary>
    /// Type of Entity (Ship or Planet)
    /// </summary>
    /// <returns>EntityType</returns>
    public EntityType EntityType {
        get {
            return type;
        }
    }

    public override string ToString()
    {
        StringBuilder s = new StringBuilder();
        s.AppendLine("");
        s.AppendLine("Entity Info");
        s.AppendLine("Type-" + this.EntityType );
        s.AppendLine("Id- " + this.Id);
        s.AppendLine("Owner- " + this.Owner);  
        s.AppendLine("");
        return s.ToString();
    }
}